-- |
module Phone where

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and
type Presses = Int

newtype DaPhone = DaPhone [(Digit, [Digit])]
  deriving (Eq, Show)

phone :: DaPhone
phone =
  DaPhone
    [ ('1', []),
      ('2', "ABC"),
      ('3', "DEF"),
      ('4', "GHI"),
      ('5', "JKL"),
      ('6', "MNO"),
      ('7', "PQRS"),
      ('8', "TUV"),
      ('9', "WXYZ"),
      ('*', "^*"),
      ('0', "+_0"),
      ('#', ".,#")
    ]
