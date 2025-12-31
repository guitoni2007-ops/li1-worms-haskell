module Worms
  ( Worms(..)
  , Menu(..)
  , Match(..)
  , Bracket(..)
  , pairUp
  , buildRounds
  , seedBracketFromList
  , advanceWinnerInBracket
  , Input(..)
  , applyInputsToEstado
  , initialState
  , criaEstadoInicial
  , criaEstadoForMatch
  ) where

import qualified Labs2025
import Data.List (nub)
import Tarefa0_geral (movePosicao, ePosicaoMatrizValida)
import Tarefa0_2025 (ePosicaoEstadoLivre)
import Labs2025

-- Menu do jogo
data Menu = MainMenu | CountrySelect | Game
  deriving (Show, Eq)

-- Match / Bracket
data Match a = Match a a
  deriving (Show, Eq)

type Round a = [Match a]

data Bracket a = Bracket
  { rounds :: [Round a]
  } deriving (Show, Eq)

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

-- Inputs do jogador
data Input = IUp | IDown | ILeft | IRight | IFire
  deriving (Eq, Show, Read)

-- Aplica uma lista de Inputs à minhoca com índice dado (0-based).
-- Se o índice for inválido, devolve o estado inalterado.
applyInputsToEstado :: Int -> [Input] -> Labs2025.Estado -> Labs2025.Estado
applyInputsToEstado _ [] est = est
applyInputsToEstado idx inputs est =
  let inputs' = nub inputs
      mInput = case inputs' of
                 (i:_) -> Just i
                 []    -> Nothing
  in case mInput of
       Nothing -> est
       Just inp -> applySingleInputIdx idx inp est

-- Aplica um único Input à minhoca no índice idx.
-- Movimentos (IUp/IDown/ILeft/IRight) tentam mover a minhoca; IFire é ignorado aqui.
applySingleInputIdx :: Int -> Input -> Labs2025.Estado -> Labs2025.Estado
applySingleInputIdx idx inp est =
  let minhocas = Labs2025.minhocasEstado est
  in if idx < 0 || idx >= length minhocas
       then est
       else
         let (before, targetAndAfter) = splitAt idx minhocas
         in case targetAndAfter of
              [] -> est
              (m:after) ->
                case Labs2025.posicaoMinhoca m of
                  Nothing -> est  -- minhoca "ausente" (jetpack/escavadora), não move
                  Just pos ->
                    if inp == IFire
                      then est
                      else
                        let dir = case inp of
                                    IUp    -> Labs2025.Norte
                                    IDown  -> Labs2025.Sul
                                    ILeft  -> Labs2025.Oeste
                                    IRight -> Labs2025.Este
                                    _      -> Labs2025.Norte
                            newPos = movePosicao dir pos
                            mapa = Labs2025.mapaEstado est
                            -- para verificar se a nova posição está livre, marcamos temporariamente
                            -- a minhoca alvo como ausente (Nothing) para não colidir consigo própria
                            tempMinhocas = before ++ (m { Labs2025.posicaoMinhoca = Nothing } : after)
                            estTemp = est { Labs2025.minhocasEstado = tempMinhocas }
                            canMove = ePosicaoMatrizValida newPos mapa && ePosicaoEstadoLivre newPos estTemp
                        in if not canMove
                             then est
                             else
                               let m' = m { Labs2025.posicaoMinhoca = Just newPos }
                                   minhocas' = before ++ (m' : after)
                               in est { Labs2025.minhocasEstado = minhocas' }

-- Tipo principal do estado da aplicação (Worms)
-- contém campos de UI e controlo do jogo
data Worms = Worms
  { menu             :: Menu
  , countryIndex     :: Int
  , hoverArrow       :: Int
  , hoverMain        :: Int
  , hoverFlag        :: Bool
  , hoverPlay        :: Bool
  , tournament       :: Bool
  , showWhite        :: Bool
  , bracket          :: Maybe (Bracket String)
  , position         :: (Float, Float)
  , estadoJogo       :: Maybe Labs2025.Estado
  , pendingInputs    :: [Input]
  , tickAcc          :: Float
  , currentTurn      :: Int
  , turnTicksLeft    :: Int
  , turnDuration     :: Int
  , currentMatch     :: Maybe (Match String)  -- match actualmente a jogar
  , lastWinner       :: Maybe String          -- último vencedor (nome do país)
  -- novos campos para estatísticas
  , lastMatchInitial :: Maybe Labs2025.Estado
  , lastMatchFinal   :: Maybe Labs2025.Estado
  , showStatistics   :: Bool
  } deriving (Show, Eq)

-- Estado inicial do wrapper Worms (UI + jogo)
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
  , currentTurn = 0
  , turnTicksLeft = 30
  , turnDuration = 30
  , currentMatch = Nothing
  , lastWinner = Nothing
  , lastMatchInitial = Nothing
  , lastMatchFinal = Nothing
  , showStatistics = False
  }

-- criaEstadoInicial: devolve um Estado de jogo com mapa, objetos e minhocas.
-- Aqui definimos as minhocas com inventário por-minhoca: 3 bazucas, 2 minas, 1 jetpack, 1 escavadora, 1 dinamite
criaEstadoInicial :: Labs2025.Estado
criaEstadoInicial = Labs2025.Estado
    { Labs2025.mapaEstado =[[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Ar,Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Ar,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Ar,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra]
        ,[Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Ar]
        ,[Terra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Ar,Ar,Ar,Ar,Ar,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Agua,Agua,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Terra,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Agua,Agua,Terra,Terra,Terra,Terra,Pedra,Terra,Ar,Ar,Terra,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Pedra,Terra,Terra,Terra,Terra,Pedra,Pedra,Ar,Ar,Terra,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Pedra,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Pedra,Terra,Pedra,Terra,Pedra,Pedra,Ar,Ar,Ar,Ar,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Agua,Agua,Agua,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Pedra,Terra,Pedra,Pedra,Ar,Ar,Ar,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Agua,Agua,Agua,Terra,Terra,Terra,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Pedra,Terra,Pedra,Pedra,Ar,Ar,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar,Ar,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Pedra,Pedra,Terra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Ar,Ar,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Pedra,Terra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Terra,Terra,Pedra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Terra,Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Pedra,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua,Agua,Terra,Pedra,Pedra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua,Terra,Terra,Terra,Terra,Terra]
        ,[Terra,Terra,Terra,Terra,Pedra,Ar,Pedra,Agua,Agua,Agua,Agua,Terra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Pedra,Agua,Agua,Agua,Agua,Agua,Terra,Terra,Terra,Terra,Terra]
        ]
    , Labs2025.objetosEstado =
        [ Labs2025.Barril { Labs2025.posicaoBarril = (25,5), Labs2025.explodeBarril = False }
        , Labs2025.Barril { Labs2025.posicaoBarril = (10,36), Labs2025.explodeBarril = False }
        , Labs2025.Barril { Labs2025.posicaoBarril = (26,33), Labs2025.explodeBarril = False }
        ]
    , Labs2025.minhocasEstado =
        [ Labs2025.Minhoca { Labs2025.posicaoMinhoca = Just (7,13)
                           , Labs2025.vidaMinhoca = Labs2025.Viva 100
                           , Labs2025.jetpackMinhoca = 1
                           , Labs2025.escavadoraMinhoca = 1
                           , Labs2025.bazucaMinhoca = 3
                           , Labs2025.minaMinhoca = 2
                           , Labs2025.dinamiteMinhoca = 1
                           }
        , Labs2025.Minhoca { Labs2025.posicaoMinhoca = Just (22,36)
                           , Labs2025.vidaMinhoca = Labs2025.Viva 100
                           , Labs2025.jetpackMinhoca = 1
                           , Labs2025.escavadoraMinhoca = 1
                           , Labs2025.bazucaMinhoca = 3
                           , Labs2025.minaMinhoca = 2
                           , Labs2025.dinamiteMinhoca = 1
                           }
        ]
    }

-- criaEstadoForMatch: versão temporária que devolve o estado inicial completo.
-- Mantive a implementação simples (como antes). Se quiseres que posicione as duas minhocas
-- do match em posições específicas, diz-me que eu altero aqui.
criaEstadoForMatch :: Match String -> Labs2025.Estado
criaEstadoForMatch _ = criaEstadoInicial








