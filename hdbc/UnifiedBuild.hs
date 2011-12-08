-- file: mysql.hs
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

main :: IO ()
main = do conn <- connectMySQL defaultMySQLConnectInfo {
            mysqlUser     = "root",
            mysqlPassword = "test",
            mysqlDatabase = "test"
            }

          rows <- quickQuery' conn "SELECT root_cause, logs FROM build WHERE root_cause IS NOT NULL" []
          --forM_ rows $ \row -> putStrLn $ show row
          forM_ (fromSqlRows rows) $ \row -> putStrLn $ show row
          
fromSqlRows :: [[SqlValue]] -> [[String]]
fromSqlRows xs = map fromSqlRow xs

fromSqlRow :: [SqlValue] -> [String]
fromSqlRow xs = map cleanValues $ map fromSql xs

cleanValues :: String -> String
cleanValues xs = filter (\y -> y == ' ' || y >= '1' && y <= 'z') xs
