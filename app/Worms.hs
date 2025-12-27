module Worms
  ( Worms(..)
  , Menu(..)
  , initialState
  ) where

data Menu = MainMenu | CountrySelect | Game
  deriving (Show, Eq)

data Worms = Worms
  { menu         :: Menu
  , countryIndex :: Int
  , hoverArrow   :: Int    -- -1 = nenhum, 0 = left, 1 = right
  , hoverMain    :: Int    -- -1 = nenhum, 0 = Classic, 1 = Tournament, 2 = Exit
  , hoverFlag    :: Bool   -- True quando o rato está sobre a bandeira
  } deriving (Show, Eq)

initialState :: Worms
initialState = Worms
  { menu = MainMenu
  , countryIndex = 0
  , hoverArrow = -1
  , hoverMain = -1
  , hoverFlag = False
  }














