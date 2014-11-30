{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Time
import System.Locale
import qualified Data.Text.IO as T
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BS


url = "https://egov.uscis.gov/cris/Dashboard/CaseStatus.do"

defConfig = defaultConfig { wdCapabilities = allCaps }

page :: WD ()
page = do
  openPage url
  receipt <- findElem (ById "receipt")
  sendKeys "WAC1414452942" receipt
  submit receipt
  waitUntil 10000 $ do
    title <- findElem (ByCSS "#caseStatus > h3") >>= getText
    current <- findElem (ByCSS "#caseStatus .controls h4") >>= getText
    ss <- screenshot
    liftIO $ do
      now <- fmap (formatCalendarTime defaultTimeLocale "%Y-%m-%d")  (getClockTime >>= toCalendarTime )
      print title >> T.putStrLn current
      BS.writeFile ("screenshot_" ++ now ++ ".png") ss


main :: IO ()
main = run page

run :: WD () -> IO ()
run = runSession defConfig . finallyClose
