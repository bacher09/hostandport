module Network.HostAndPort (
    isIPv4Address,
    isIPv6Address
) where
import Text.Parsec
import Control.Applicative hiding((<|>), many)
import Control.Monad

type Parser = Parsec String ()


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
ipv4address = concat <$> sequence
    [byteNum, string ".", byteNum, string ".",
     byteNum, string ".", byteNum]


hexShortNum :: Parser String
hexShortNum = countMinMax 1 4 hexDigit


port :: Parser String
port = limitedInt 65535 "Port number to large"


ipv6address :: Parser String
ipv6address = do
    let ipv6variants = (try <$> skippedAtMiddle)
                        ++ (try <$> skippedAtBegin)
                        ++ [try $ consequence [partNum 6, last2 False]]
                        ++ [last2 False]
    choice ipv6variants <?> "IPv6 address"
  where
    h4s = (++) <$> hexShortNum <*> string ":"
    sh4 = (++) <$> string ":" <*> hexShortNum
    execNum 0 = return ""
    execNum n = do
        r <- count (n - 1) h4s
        return $ concat r
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
        consequence [partNum 4, string "::", maybeNum 1, last2 True]]


ipv6addressWithScope :: Parser String
ipv6addressWithScope = consequence [ipv6address, option "" scope]
  where
    scope = consequence [string "%", many1 asciiAlphaNum]


isParsed :: Parser a -> String -> Bool
isParsed p s = case runParser p () "" s of
    (Right _) -> True
    (Left _) -> False


ended :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
ended p = p >>= \v -> eof >> return v


isIPv4Address :: String -> Bool
isIPv4Address = isParsed $ ended ipv4address


isIPv6Address :: String -> Bool
isIPv6Address = isParsed $ ended ipv6addressWithScope


isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')


isAsciiNum :: Char -> Bool
isAsciiNum c = (c >= '0' && c <= '9')


isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isAsciiNum c


asciiAlphaNum :: Parser Char
asciiAlphaNum = satisfy isAsciiAlphaNum
