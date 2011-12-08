-- file: chp03/BookStore.hs

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 908381928 "Algebra of Programming" ["Richard Bird","Oege de Moor"]

