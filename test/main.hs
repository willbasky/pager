import qualified Data.ByteString.Lazy as B
import Data.Conduit.Binary
import Paths_pager
import System.Pager

testConduit :: FilePath ->  IO ()
testConduit fnom =
  sendToPagerConduit (sourceFile fnom)

test :: FilePath -> IO ()
test fp = B.readFile fp >>= sendToPager

main =
  do fnom <- getDataFileName "LICENSE"
     putStrLn "Hit Return to start the conduit-free test"
     _ <- getLine
     test fnom
     putStrLn "Hit Return to start the conduit test"
     _ <- getLine
     testConduit fnom
