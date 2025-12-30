module Worms
  ( Worms(..)
  , Menu(..)
  , initialState
  , Match(..)
  , Bracket(..)
  , pairUp
  , buildRounds
  , seedBracketFromList
  , advanceWinnerInBracket
  , Input(..)
  , applyInputsToEstado
  , criaEstadoInicial
  ) where

import qualified Labs2025
import Data.List (nub)
import Tarefa0_geral (movePosicao, ePosicaoMatrizValida)
import Tarefa0_2025 (ePosicaoEstadoLivre)

data Menu = MainMenu | CountrySelect | Game
  deriving (Show, Eq)

data Match a = Match a a
  deriving (Show, Eq)

type Round a = [Match a]

data Bracket a = Bracket
  { rounds :: [Round a]
  } deriving (Show, Eq)

data Worms = Worms
  { menu         :: Menu
  , countryIndex :: Int
  , hoverArrow   :: Int
  , hoverMain    :: Int
  , hoverFlag    :: Bool
  , hoverPlay    :: Bool
  , tournament   :: Bool
  , showWhite    :: Bool
  , bracket      :: Maybe (Bracket String)
  , position     :: (Float, Float)
  , estadoJogo   :: Maybe Labs2025.Estado
  , pendingInputs :: [Input]
  , tickAcc      :: Float
  } deriving (Show, Eq)

initialState :: Worms
initialState = Worms
  { menu = MainMenu
  , countryIndex = 0
  , hoverArrow = -1
  , hoverMain = -1
  , hoverFlag = False
  , hoverPlay = False
  , tournament = False
  , showWhite = False
  , bracket = Nothing
  , position = (0,0)
  , estadoJogo = Nothing
  , pendingInputs = []
  , tickAcc = 0.0
  }

pairUp :: [a] -> [Match a]
pairUp [] = []
pairUp (x:y:rest) = Match x y : pairUp rest
pairUp [x] = [Match x x]

buildRounds :: [a] -> [Round a]
buildRounds [] = []
buildRounds ps = go (pairUp ps)
  where
    go cur
      | length cur <= 1 = [cur]
      | otherwise =
          let winnersPlaceholder = map (\(Match a _) -> a) cur
              next = pairUp winnersPlaceholder
          in cur : go next

seedBracketFromList :: [String] -> Bracket String
seedBracketFromList countries = Bracket { rounds = buildRounds countries }

advanceWinnerInBracket :: Eq a => Bracket a -> Int -> Int -> a -> Bracket a
advanceWinnerInBracket b rIdx mIdx winner =
  let rs = rounds b
      updateRound i r
        | i == rIdx =
            let updated = take mIdx r ++ [replaceMatch (r !! mIdx) winner] ++ drop (mIdx + 1) r
            in updated
        | otherwise = r
      replaceMatch (Match _ _) w = Match w w
      newRounds = zipWith updateRound [0..] rs
  in b { rounds = newRounds }

data Input = IUp | IDown | ILeft | IRight | IFire
  deriving (Eq, Show, Read)

applyInputsToEstado :: [Input] -> Labs2025.Estado -> Labs2025.Estado
applyInputsToEstado [] est = est
applyInputsToEstado inputs est =
  let inputs' = nub inputs
      mInput = case inputs' of
                 (i:_) -> Just i
                 []    -> Nothing
  in case mInput of
       Nothing -> est
       Just inp -> applySingleInput inp est

applySingleInput :: Input -> Labs2025.Estado -> Labs2025.Estado
applySingleInput inp est =
  case Labs2025.minhocasEstado est of
    [] -> est
    (m:ms) ->
      case Labs2025.posicaoMinhoca m of
        Nothing -> est
        Just pos ->
          let dir = case inp of
                      IUp    -> Labs2025.Norte
                      IDown  -> Labs2025.Sul
                      ILeft  -> Labs2025.Oeste
                      IRight -> Labs2025.Este
                      IFire  -> Labs2025.Norte
              newPos = movePosicao dir pos
              mapa = Labs2025.mapaEstado est
              canMove = ePosicaoMatrizValida newPos mapa && ePosicaoEstadoLivre newPos est
          in if inp == IFire
               then est
               else if canMove
                      then let m' = m { Labs2025.posicaoMinhoca = Just newPos }
                               minhocas' = m' : ms
                           in est { Labs2025.minhocasEstado = minhocas' }
                      else est

criaEstadoInicial :: Labs2025.Estado
criaEstadoInicial = Labs2025.Estado
  { Labs2025.mapaEstado =
      [ [Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Pedra,Labs2025.Pedra,Labs2025.Ar,Labs2025.Ar,Labs2025.Ar]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Pedra,Labs2025.Pedra,Labs2025.Agua,Labs2025.Agua,Labs2025.Agua]
      , [Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Terra,Labs2025.Pedra,Labs2025.Pedra,Labs2025.Agua,Labs2025.Agua,Labs2025.Agua]
      ]
  , Labs2025.objetosEstado =
      [ Labs2025.Barril { Labs2025.posicaoBarril = (6,5), Labs2025.explodeBarril = False }
      , Labs2025.Barril { Labs2025.posicaoBarril = (7,6), Labs2025.explodeBarril = False }
      , Labs2025.Disparo { Labs2025.posicaoDisparo = (5,3), Labs2025.direcaoDisparo = Labs2025.Noroeste, Labs2025.tipoDisparo = Labs2025.Bazuca, Labs2025.tempoDisparo = Nothing, Labs2025.donoDisparo = 0 }
      , Labs2025.Disparo { Labs2025.posicaoDisparo = (2,0), Labs2025.direcaoDisparo = Labs2025.Norte, Labs2025.tipoDisparo = Labs2025.Mina, Labs2025.tempoDisparo = Nothing, Labs2025.donoDisparo = 0 }
      , Labs2025.Disparo { Labs2025.posicaoDisparo = (3,1), Labs2025.direcaoDisparo = Labs2025.Norte, Labs2025.tipoDisparo = Labs2025.Dinamite, Labs2025.tempoDisparo = Just 1, Labs2025.donoDisparo = 0 }
      ]
  , Labs2025.minhocasEstado =
      [ Labs2025.Minhoca { Labs2025.posicaoMinhoca = Just (7,7), Labs2025.vidaMinhoca = Labs2025.Viva 100, Labs2025.jetpackMinhoca = 1, Labs2025.escavadoraMinhoca = 1, Labs2025.bazucaMinhoca = 1, Labs2025.minaMinhoca = 1, Labs2025.dinamiteMinhoca = 1 } ]
  }
















  





















