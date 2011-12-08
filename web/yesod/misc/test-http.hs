import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy as L

main = simpleHttp "http://www.haskell.org/" >>= L.putStr
