module Main where
import Criterion.Main
import Network.HostAndPort


main :: IO ()
main = defaultMain [
        bgroup "ipv4" [ bench "127.0.0.1" $ whnf isIPv4Address "127.0.0.1"
                      , bench "bad" $ whnf isIPv4Address "22222.2222.222.22"]
        , bgroup "ipv6" [ bench "::1" $ whnf isIPv6Address "::1"
                        , bench "vinnica.in" $ whnf isIPv6Address "2605:2700:0:2::4713:9eef"
                        , bench "full" $ whnf isIPv6Address "1:2:3:4:5:6:7:8"
                        , bench "with-ipv4" $ whnf isIPv6Address "1:2:3:4:5:6:127.0.0.1"
                        , bench "bad" $ whnf isIPv6Address "2605:2700:0:2::4713:9eef"]
        , bgroup "HostAndPort" [ bench "127.0.0.1" $ nf hostAndPort "127.0.0.1"
                               , bench "localhost:9090" $ nf hostAndPort "localhost:9090"
                               , bench "ipv6-and-port" $ nf hostAndPort "[2605:2700:0:2::4713:9eef]:26000"]
        ]
