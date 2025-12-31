
module Main where

import Graphics.Gloss (Display(InWindow), Color, greyN)
import Graphics.Gloss.Interface.Pure.Game (play)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Desenhar (desenha)
import Eventos (reageEventos)
import qualified Worms as W
import Tempo (reageTempo)
import Data.Maybe (fromJust)
import System.Exit (exitFailure)

-- Janela e parâmetros
janela :: Display
janela = InWindow "Worms World Cup" (1920,1080) (100, 100)

fundo :: Color
fundo = greyN 0.5

fr :: Int
fr = 60  -- render a 60 FPS

flagFilesMenu :: [FilePath]
flagFilesMenu =
  [ "flagsportugal.png", "flagsbrasil.png", "flagsargentina.png", "flagsfranca.png"
  , "flagsalemanha.png", "flagsespanha.png", "flagsinglaterra.png", "flagsjapao.png"
  ]

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
  mTitle         <- loadJuicyPNG "simbolo.png"

  -- flags
  mFlagsMenu     <- mapM loadJuicyPNG flagFilesMenu
  mFlagsBracket  <- mapM loadJuicyPNG flagFilesBracket

  -- imagens das minhocas: nomes em minúsculas conforme pedido
  let wormFiles =
        [ "wormsportugal.png"
        , "wormsbrasil.png"
        , "wormsargentina.png"
        , "wormsfranca.png"
        , "wormsalemanha.png"
        , "wormsespanha.png"
        , "wormsinglaterra.png"
        , "wormsjapao.png"
        ]
  mWormsMaybe <- mapM loadJuicyPNG wormFiles

  -- verifica se alguma imagem falhou a carregar
  let missingWorms = [f | (f, Nothing) <- zip wormFiles mWormsMaybe]
      missingFlagsMenu = [f | (f, Nothing) <- zip flagFilesMenu mFlagsMenu]
      missingFlagsBracket = [f | (f, Nothing) <- zip flagFilesBracket mFlagsBracket]
      missingTitle = if mTitle == Nothing then ["simbolo.png"] else []
      missingAll = missingTitle ++ missingWorms ++ missingFlagsMenu ++ missingFlagsBracket

  if not (null missingAll)
    then do
      putStrLn "Erro: as seguintes imagens não foram encontradas ou não puderam ser carregadas:"
      mapM_ putStrLn missingAll
      putStrLn "Verifica nomes (case sensitive), caminhos e se os ficheiros são PNG válidos."
      exitFailure
    else do
      -- extrai as imagens (são todas Just)
      let mTitle' = fromJust mTitle
          mFlagsMenu' = map fromJust mFlagsMenu
          mFlagsBracket' = map fromJust mFlagsBracket
          mWorms = map fromJust mWormsMaybe

      mBarril        <- loadJuicyPNG "barrilnormal.png" >>= ensure "barrilnormal.png"
      mBarrilExplode <- loadJuicyPNG "barrilexplodir.png" >>= ensure "barrilexplodir.png"
      mMina          <- loadJuicyPNG "minanormal.png" >>= ensure "minanormal.png"
      mJetpack       <- loadJuicyPNG "jetpacknormal.png" >>= ensure "jetpacknormal.png"
      mDinamite      <- loadJuicyPNG "dinamitenormal.png" >>= ensure "dinamitenormal.png"
      mBazuca        <- loadJuicyPNG "bazucanormal.png" >>= ensure "bazucanormal.png"
      mEscavadora    <- loadJuicyPNG "escavadoranormal.png" >>= ensure "escavadoranormal.png"
      mMenuBg        <- loadJuicyPNG "fundomenu.png" >>= ensure "fundomenu.png"
      mBracketBg     <- loadJuicyPNG "fundobracket.png" >>= ensure "fundobracket.png"
      mGameBg        <- loadJuicyPNG "fundojogo.png" >>= ensure "fundojogo.png"

      let b = W.seedBracketFromList countryNames
          it :: W.Worms
          it = W.initialState
                 { W.bracket    = Just b
                 , W.tournament = True
                 , W.estadoJogo = Nothing
                 }

      play janela fundo fr it
        (\w -> desenha (Just mMenuBg) (Just mBracketBg) (Just mGameBg) (Just mTitle') (map Just mFlagsMenu') (map Just mFlagsBracket') (map Just mWorms) (Just mBarril) (Just mBarrilExplode) (Just mMina) (Just mJetpack) (Just mDinamite) (Just mBazuca) (Just mEscavadora) w)
        reageEventos
        reageTempo

-- helper: falha com mensagem se Nothing
ensure :: FilePath -> Maybe a -> IO a
ensure fname Nothing = do
  putStrLn $ "Erro: não foi possível carregar: " ++ fname
  exitFailure
ensure _ (Just x) = return x






































