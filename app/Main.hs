module Main where

import Graphics.Gloss (Display(InWindow), Color, greyN)
import Graphics.Gloss.Interface.Pure.Game (play)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Desenhar (desenha)
import Eventos (reageEventos)
import qualified Worms as W
import Tempo (reageTempo)

-- Janela e parâmetros
janela :: Display
janela = InWindow "Worms World Cup" (1920,1080) (100, 100)

fundo :: Color
fundo = greyN 0.5

fr :: Int
fr = 60  -- render a 60 FPS

-- ficheiros das bandeiras para o menu/seleção (quando clicas em Tournament -> escolha)
flagFilesMenu :: [FilePath]
flagFilesMenu =
  [ "flagsportugal.png", "flagsbrasil.png", "flagsargentina.png", "flagsfranca.png"
  , "flagsalemanha.png", "flagsespanha.png", "flagsinglaterra.png", "flagsjapao.png"
  ]

-- ficheiros das bandeiras para a bracket (versões específicas para o ecrã da bracket)
flagFilesBracket :: [FilePath]
flagFilesBracket =
  [ "flagsportugalbracket.png", "flagsbrasilbracket.png", "flagsargentinabracket.png", "flagsfrancabracket.png"
  , "flagsalemanhabracket.png", "flagsespanhabracket.png", "flagsinglaterrabracket.png", "flagsjapaobracket.png"
  ]

countryNames :: [String]
countryNames =
  [ "Portugal", "Brasil", "Argentina", "Franca"
  , "Alemanha", "Espanha", "Inglaterra", "Japao"
  ]

main :: IO ()
main = do
  -- imagens do jogo
  mTitle         <- loadJuicyPNG "Worms world cup.png"

  -- duas listas de flags: uma para o menu/seleção e outra para a bracket
  mFlagsMenu     <- mapM loadJuicyPNG flagFilesMenu
  mFlagsBracket  <- mapM loadJuicyPNG flagFilesBracket

  mWorm          <- loadJuicyPNG "wormsPortugal.png"
  mBarril        <- loadJuicyPNG "barrilnormal.png"
  mBarrilExplode <- loadJuicyPNG "barrilexplodir.png"
  mMina          <- loadJuicyPNG "minanormal.png"
  mJetpack       <- loadJuicyPNG "jetpacknormal.png"
  mDinamite      <- loadJuicyPNG "dinamitenormal.png"
  mBazuca        <- loadJuicyPNG "bazucanormal.png"
  mEscavadora    <- loadJuicyPNG "escavadoranormal.png"

  -- fundo do menu pedido pelo utilizador
  mMenuBg        <- loadJuicyPNG "fundomenu.png"

  -- fundo específico para o ecrã das brackets
  mBracketBg     <- loadJuicyPNG "fundobracket.png"

  let b = W.seedBracketFromList countryNames
      -- estado inicial da aplicação (UI). estadoJogo fica Nothing até o utilizador clicar Play.
      it :: W.Worms
      it = W.initialState
             { W.bracket    = Just b
             , W.tournament = True
             , W.estadoJogo = Nothing
             }

  -- ordem correcta de argumentos para 'play':
  -- play display background fps initialWorld render handleEvent step
  play janela fundo fr it
    (\w -> desenha mMenuBg mBracketBg mTitle mFlagsMenu mFlagsBracket mWorm mBarril mBarrilExplode mMina mJetpack mDinamite mBazuca mEscavadora w)
    reageEventos
    reageTempo


































