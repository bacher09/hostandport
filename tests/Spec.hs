module Main where
import Test.Hspec
import Network.HostAndPort
import Text.Printf


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "isIPv4Address" $ do
        valid isIPv4Address "127.0.0.1"
        valid isIPv4Address "255.255.255.255"

        invalid isIPv4Address " 127.0.0.1"
        invalid isIPv4Address "127.0.0.1 "

        invalid isIPv4Address "9.9.256.1"

    describe "isIPv6Address" $ do
        valid isIPv6Address "::1"
        valid isIPv6Address "::1:2"
        valid isIPv6Address "::1:2:3"
        valid isIPv6Address "::1:2:3:4"
        valid isIPv6Address "::1:2:3:4:5"
        valid isIPv6Address "::1:2:3:4:5:6"
        valid isIPv6Address "::1:2:3:4:5:6:7"
        valid isIPv6Address "1:2:3:4:5:6:7:8"
        valid isIPv6Address "::192.168.1.10"
        valid isIPv6Address "1::192.168.1.10"
        valid isIPv6Address "1:2::192.168.1.10"
        valid isIPv6Address "1:2:3::192.168.1.10"
        valid isIPv6Address "1:2:3:4::192.168.1.10"
        valid isIPv6Address "1:2:3:4:5::192.168.1.10"
        valid isIPv6Address "::1:192.168.1.10"
        valid isIPv6Address "::1:2:192.168.1.10"
        valid isIPv6Address "::1:2:3:192.168.1.10"
        valid isIPv6Address "::1:2:3:4:192.168.1.10"
        valid isIPv6Address "::1:2:3:4:5:192.168.1.10"
        valid isIPv6Address "::1:2:3:4:5:192.168.1.10"
        valid isIPv6Address "ff::192.168.1.10"
        valid isIPv6Address "1:2:3:4:5:6:127.0.0.1"
        valid isIPv6Address "1::"
        valid isIPv6Address "1:2::"
        valid isIPv6Address "1:2:3::"
        valid isIPv6Address "1:2:3:4::"
        valid isIPv6Address "1:2:3:4:5::"
        valid isIPv6Address "1:2:3:4:5:6::"
        valid isIPv6Address "1:2:3:4:5:6:7::"
        valid isIPv6Address "1::f"
        valid isIPv6Address "1:2::f"
        valid isIPv6Address "1:2:3::f"
        valid isIPv6Address "1:2:3:4::f"
        valid isIPv6Address "1:2:3:4:5::f"
        valid isIPv6Address "1:2:3:4:5:6::f"
        valid isIPv6Address "1:2:3:4:5::6:f"
        valid isIPv6Address "1:2:3:4::5:6:f"
        valid isIPv6Address "1:2:3::4:5:6:f"
        valid isIPv6Address "1:2::3:4:5:6:f"
        valid isIPv6Address "1::2:3:4:5:6:f"
        valid isIPv6Address "::1:2:3:4:5:6:f"
        valid isIPv6Address "::ffff:0:255.255.255.255"
        valid isIPv6Address "fe80::7:8%eth0"
        valid isIPv6Address "fe80::7:8%11"

        invalid isIPv6Address "::"
        invalid isIPv6Address "1:2:3:4:5:6:7:8:9"
        invalid isIPv6Address "1:2:3:4:5:6:7:"
        invalid isIPv6Address "1:2:3:4:5:6:7"
        invalid isIPv6Address ":1:2:3:4:5:6:7"
        invalid isIPv6Address "1:2:3:4:5:6:7:10.0.0.1"
        invalid isIPv6Address "::1:2:3:4:5:6:7:8"
        invalid isIPv6Address "1:2:3:4:5:6::192.168.1.10"
        invalid isIPv6Address "1:2:3:4:5:6:7:8::"
  where
    valid f s = it (printf "valid \"%s\"" s) (f s `shouldBe` True)
    invalid f s = it (printf "invalid \"%s\"" s) (f s `shouldBe` False)
