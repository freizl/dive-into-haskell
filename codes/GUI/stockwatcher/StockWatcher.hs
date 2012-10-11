module Main where

-- Find listStore example from ls /usr/share/doc/gtk2hs-doc/examples/treeList/
-- TODO: setup .cabal file

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New
import Random

data Stock = Stock { symbol :: String, price :: Float }

main = do
  initGUI
    
  -- load up the glade file
  dialogXmlM <- xmlNew "stockwatcher.glade"
  let dialogXml = case dialogXmlM of
        (Just dialogXml) -> dialogXml
        Nothing -> error "Failed to find the glade file \"stockwatcher.glade\" in the current directory"
	 
  -- get a handle on a couple widgets from the glade file
  window     <- xmlGetWidget dialogXml castToWindow "mainWindow"
  view       <- xmlGetWidget dialogXml castToTreeView "stockTable"	
  button     <- xmlGetWidget dialogXml castToButton "addButton"
  inputEntry <- xmlGetWidget dialogXml castToEntry "inputEntry"
	 
  store <- New.listStoreNew []
  New.treeViewSetModel view store
  setupTable view store
  
  -- do something with the widgets, just to prove it works
  --  onClicked button $ getNewStock inputEntry >>= New.listStoreAppend store >> entrySetText inputEntry ""
  onClicked button $ onAddHandler inputEntry store
  onDestroy window mainQuit
	 
  New.treeViewSetReorderable view True
  
  -- show everything
  widgetShowAll window
  mainGUI
  
  -- TODO: how to define type for the addHandler??
  -- TODO: how to define handler in where therefore donot need to pass parameter.
  -- onAddHandler :: Entry -> ListStore -> IO ()
onAddHandler inputEntry store = getNewStock inputEntry >>= New.listStoreAppend store >> entrySetText inputEntry ""

  -- TODO while inputText is empty
getNewStock :: Entry -> IO Stock
getNewStock inputEntry = do
  inputText <- entryGetText inputEntry
  randomPrice <- randomRIO (1,100)
  return Stock { symbol = inputText, price = randomPrice }

setupTable view model = do
  New.treeViewSetHeadersVisible view True
  
  render1 <- New.cellRendererTextNew
  col1 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col1 render1 True
  New.cellLayoutSetAttributes col1 render1 model $ \row -> [ New.cellText := symbol row ]
  New.treeViewColumnSetTitle col1 "Symbol"
  New.treeViewAppendColumn view col1
  
  render2 <- New.cellRendererTextNew
  col2 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col2 render2 True
  New.cellLayoutSetAttributes col2 render2 model $ \row -> [ New.cellText := show $ price row ]
  New.treeViewColumnSetTitle col2 "Price"
  New.treeViewAppendColumn view col2
