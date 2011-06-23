-- file: mysql.hs
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
main = do conn <- connectMySQL defaultMySQLConnectInfo {
                     mysqlUser     = "root",
                     mysqlPassword = "navichina",
                     mysqlDatabase = "swingshowcase"
                  }

          rows <- quickQuery' conn "SELECT * from customer" []
          forM_ rows $ \row -> putStrLn $ show row

-- conn <- connectMySQL defaultMySQLConnectInfo { mysqlHost = "localhost", mysqlUser = "root", mysqlPassword = "navichina", mysqlDatabase="swingshowcase" }
