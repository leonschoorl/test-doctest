{-# OPTIONS_GHC -fplugin DummyPlugin   #-}

module MyLib where


-- | myNot
-- >>> myNot False
-- True
myNot :: Bool -> Bool
myNot = not
