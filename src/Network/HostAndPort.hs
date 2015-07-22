module Network.HostAndPort (
    ConnectionDetail(..),
    RemoteAddr,
    isIPv4Address,
    isIPv6Address,
    hostAndPort,
    detailedHostAndPort,
    maybeHostAndPort,
    defaultHostAndPort
) where
import Text.Parsec
import Control.Applicative hiding((<|>), many)
import Control.Monad
import Data.Maybe


data ConnectionDetail a = IPv4Address a
                        | IPv6Address a
                        | HostName a
    deriving(Show, Eq, Ord)


type Parser = Parsec String ()
type RemoteAddr = ConnectionDetail String


countMinMax :: (Stream s m t) => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countMinMax m x p
    | m > 0 = do
        f <- p
        end <- countMinMax (m - 1) (x - 1) p
        return $ f : end
    | x <= 0 = return []
    | otherwise = option [] $ do
        f <- p
        end <- countMinMax 0 (x - 1) p
        return $ f : end


intDigits :: Int -> Int
intDigits = length . show


limitedInt :: Int -> String -> Parser String
limitedInt x e = do
    b <- countMinMax 1 (intDigits x) digit
    if (read b :: Int) > x
        then fail e
        else return b


byteNum :: Parser String
byteNum = limitedInt 255 "Value to large"


consequence :: (Monad m) => [m [a]] -> m [a]
consequence v = liftM concat $ sequence v


ipv4address :: Parser String
ipv4address = consequence [
    byteNum, string ".",
    byteNum, string ".",
    byteNum, string ".", byteNum] <?> "bad IPv4 address"


hexShortNum :: Parser String
hexShortNum = countMinMax 1 4 hexDigit


port :: Parser String
port = limitedInt 65535 "Port number to large"


ipv6address :: Parser String
ipv6address = do
    let ipv6variants = (try <$> skippedAtBegin)
                        ++ [try full]
                        ++ (try <$> skippedAtMiddle)
                        ++ (try <$> skippedAtEnd)
                        ++ [last2 False]
    choice ipv6variants <?> "bad IPv6 address"
  where
    h4s = (++) <$> hexShortNum <*> string ":"
    sh4 = (++) <$> string ":" <*> hexShortNum
    execNum 0 = return ""
    execNum n = concat <$> count n h4s
    partNum 0 = return ""
    partNum n = do
        f <- hexShortNum
        e <- countMinMax 0 (n - 1) (try sh4)
        return $ f ++ concat e

    maybeNum n = concat <$> countMinMax 0 n h4s
    last2f = try ipv4address <|> consequence [h4s, hexShortNum]
    last2 f = if f
        then last2f
        else choice [try $ last2f,
                     try $ consequence [string "::", hexShortNum],
                     consequence [hexShortNum, string "::"]]

    skippedAtBegin =
        map (\i -> consequence [string "::", execNum i, last2 True]) [5,4..0]

    skippedAtMiddle = [
        consequence [partNum 1, string "::", maybeNum 4, last2 True],
        consequence [partNum 2, string "::", maybeNum 3, last2 True],
        consequence [partNum 3, string "::", maybeNum 2, last2 True],
        consequence [partNum 4, string "::", maybeNum 1, last2 True],
        consequence [partNum 5, string "::", last2 True],
        consequence [partNum 6, string "::", hexShortNum]]

    skippedAtEnd = [consequence [partNum 7, string "::"]]

    full = consequence [concat <$> count 6 h4s, last2 True]


ipv6addressWithScope :: Parser String
ipv6addressWithScope = consequence [ipv6address, option "" scope]
  where
    scope = consequence [string "%", many1 asciiAlphaNum]


hostname :: Parser String
hostname = many1 $ alphaNum <|> oneOf ".-_"


isParsed :: Parser a -> String -> Bool
isParsed p s = case runParser p () "" s of
    (Right _) -> True
    (Left _) -> False


-- | This function will validate ipv4 address
-- and return True if string is valid adress
isIPv4Address :: String -> Bool
isIPv4Address = isParsed $ ipv4address <* eof


-- | Function validates ipv6 address
isIPv6Address :: String -> Bool
isIPv6Address = isParsed $ ipv6addressWithScope <* eof


isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')


isAsciiNum :: Char -> Bool
isAsciiNum c = (c >= '0' && c <= '9')


isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isAsciiNum c


asciiAlphaNum :: Parser Char
asciiAlphaNum = satisfy isAsciiAlphaNum


connectionStr :: (String -> a) ->
                 (String -> a) ->
                 (String -> a) -> Parser (a, Maybe String)
connectionStr ipv6Fun ipv4Fun hostFun = do
    addr <- try (ipv6Fun <$> ipv6str)
                <|> try (ipv4Fun <$> ipv4address)
                <|> (hostFun <$> hostname)
    p <- maybePort
    return (addr, p)
  where
    ipv6str = do
        void $ char '['
        ipv6 <- ipv6addressWithScope
        void $ char ']'
        return ipv6
    maybePort = option Nothing $ char ':' >> Just <$> port


-- | This function will parse it's argument and return either
-- `String` (`Left`) in case of error or (`ConnectionDetail String`, Maybe Port)
-- tuple (`Right`).
--
-- Examples:
--
-- >>> detailedHostAndPort "localhost"
-- Right (HostName "localhost",Nothing)
-- >>> detailedHostAndPort "[::1]:3030"
-- Right (IPv6Address "::1",Just "3030")
-- >>> detailedHostAndPort "127.0.0.1:1080"
-- Right (IPv4Address "127.0.0.1",Just "1080")
--
-- /Since/ 0.2
detailedHostAndPort :: String -> Either String (RemoteAddr, Maybe String)
detailedHostAndPort s = case runParser parser () "" s of
    (Right v) -> Right v
    (Left e) -> Left $ show e
  where
    parser = connectionStr IPv6Address IPv4Address HostName <* eof


-- | This function will parse it's argument and return either
-- `String` (`Left`) with info about error or (Host, `Maybe` Port)
-- tuple (`Right`).
--
-- Examples:
--
-- >>> hostAndPort "localhost"
-- Right ("localhost",Nothing)
-- >>> hostAndPort "[::1]:3030"
-- Right ("::1",Just "3030")
hostAndPort :: String -> Either String (String, Maybe String)
hostAndPort s = case runParser (connectionStr id id id <* eof) () "" s of
    (Right v) -> Right v
    (Left e) -> Left $ show e


-- | Function will parse argument and return Maybe (Host, Maybe Port)
--
-- Examples:
--
-- >>> maybeHostAndPort "192.168.10.12"
-- Just ("192.168.10.12",Nothing)
-- >>> maybeHostAndPort "192.168.10.12:7272"
-- Just ("192.168.10.12",Just "7272")
maybeHostAndPort :: String -> Maybe (String, Maybe String)
maybeHostAndPort s = case hostAndPort s of
    (Right v) -> Just v
    (Left _) -> Nothing


-- | Function will take default port and connection string
-- and returns Just (Host, Port) for valid input and
-- Nothing for invalid.
--
-- Examples:
--
-- >>> defaultHostAndPort "22" "my.server.com"
-- Just ("my.server.com","22")
-- >>> defaultHostAndPort "22" "my.otherserver.com:54022"
-- Just ("my.otherserver.com","54022")
-- >>> defaultHostAndPort "22" "porttobig.com:500022"
-- Nothing
defaultHostAndPort :: String                    -- ^ default Port number
                   -> String                    -- ^ connection string
                   -> Maybe (String, String)    -- ^ Maybe (Host, Port)
defaultHostAndPort p s = (\(h, mp) -> (h, fromMaybe p mp)) <$> maybeHostAndPort s
