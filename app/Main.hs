module Main where

import Graphics.Gloss
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

  -- imagens das minhocas
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

  -- imagens de vitória
  mVictoryPT <- loadJuicyPNG "fundovitoriaportugal.png"
  mVictoryBR <- loadJuicyPNG "fundovitoriabrasil.png"

  -- verifica se alguma imagem falhou a carregar
  let missingWorms = [f | (f, Nothing) <- zip wormFiles mWormsMaybe]
      missingFlagsMenu = [f | (f, Nothing) <- zip flagFilesMenu mFlagsMenu]
      missingFlagsBracket = [f | (f, Nothing) <- zip flagFilesBracket mFlagsBracket]
      missingTitle = if mTitle == Nothing then ["simbolo.png"] else []
      missingVictoryFiles = [ "fundovitoriaportugal.png" | mVictoryPT == Nothing ] ++ [ "fundovitoriabrasil.png" | mVictoryBR == Nothing ]
      missingAll = missingTitle ++ missingWorms ++ missingFlagsMenu ++ missingFlagsBracket ++ missingVictoryFiles

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
          mVictoryPT' = mVictoryPT
          mVictoryBR' = mVictoryBR

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

          -- parâmetros do botão Voltar / Estatísticas 
          backW = 160 :: Float
          backH = 48  :: Float
          backY = -380 :: Float
          statsY = backY + 70

          -- escala aplicada à imagem de vitória 
          victoryScale :: Float
          victoryScale = 0.95

          -- função de desenho que envolve 'desenha' e sobrepõe a imagem de vitória + botões
          drawAll :: Maybe Picture -> Maybe Picture -> W.Worms -> Picture
          drawAll mVpt mVbr world =
            let base = desenha (Just mMenuBg) (Just mBracketBg) (Just mGameBg) (Just mTitle') (map Just mFlagsMenu') (map Just mFlagsBracket') (map Just mWorms) (Just mBarril) (Just mBarrilExplode) (Just mMina) (Just mJetpack) (Just mDinamite) (Just mBazuca) (Just mEscavadora) world
            in if W.showStatistics world
                 then base
                 else
                   let -- imagem de vitória (apenas a imagem, sem texto)
                       victoryPicFor :: Maybe Picture -> Picture
                       victoryPicFor (Just p) = Translate 0 0 $ Scale victoryScale victoryScale p
                       victoryPicFor Nothing  = Blank

                       victoryPic =
                         case W.lastWinner world of
                           Just "Portugal" -> victoryPicFor mVpt
                           Just "Brasil"   -> victoryPicFor mVbr
                           _ -> Blank

                       -- fundo sólido para a tela de vitória 
                       victoryBg =
                         case W.lastWinner world of
                           Just "Portugal" -> case mVpt of Just _ -> Translate 0 0 $ color black $ rectangleSolid 1920 1080; Nothing -> Blank
                           Just "Brasil"   -> case mVbr of Just _ -> Translate 0 0 $ color black $ rectangleSolid 1920 1080; Nothing -> Blank
                           _ -> Blank

                       -- botão Estatisticas 
                       statsPic =
                         case W.lastWinner world of
                           Just _ ->
                             Pictures
                               [ Translate 0 statsY $ color (greyN 0.85) $ rectangleSolid backW backH
                               , Translate 0 statsY $ color black $ rectangleWire backW backH
                               , Translate (-60) (statsY - 10) $ Scale 0.20 0.20 $ color black $ Text "Estatisticas"
                               ]
                           Nothing -> Blank

                       -- botão Voltar 
                       backPic =
                         case W.lastWinner world of
                           Just _ ->
                             Pictures
                               [ Translate 0 backY $ color (greyN 0.85) $ rectangleSolid backW backH
                               , Translate 0 backY $ color black $ rectangleWire backW backH
                               , Translate (-32) (backY - 10) $ Scale 0.22 0.22 $ color black $ Text "Voltar"
                               ]
                           Nothing -> Blank

                   in case W.lastWinner world of
                        -- se houver vencedor e a imagem correspondente estiver carregada, desenha fundo + imagem + botões
                        Just "Portugal" ->
                          case mVpt of
                            Just _ -> Pictures [ victoryBg, victoryPic, statsPic, backPic ]
                            Nothing -> Pictures [ base ]  -- fallback: se imagem ausente, mostra base
                        Just "Brasil" ->
                          case mVbr of
                            Just _ -> Pictures [ victoryBg, victoryPic, statsPic, backPic ]
                            Nothing -> Pictures [ base ]
                        -- caso contrário, desenha o base normal (menu/bracket/jogo)
                        _ -> Pictures [ base ]

      play janela fundo fr it
        (drawAll mVictoryPT' mVictoryBR')
        reageEventos
        reageTempo

-- helper: falha com mensagem se Nothing
ensure :: FilePath -> Maybe a -> IO a
ensure fname Nothing = do
  putStrLn $ "Erro: não foi possível carregar: " ++ fname
  exitFailure
ensure _ (Just x) = return x











































