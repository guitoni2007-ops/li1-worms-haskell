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
  [ "imagens_projeto202526/flagsportugal.png", "imagens_projeto202526/flagsbrasil.png", "imagens_projeto202526/flagsargentina.png", "imagens_projeto202526/flagsfranca.png"
  , "imagens_projeto202526/flagsalemanha.png", "imagens_projeto202526/flagsespanha.png", "imagens_projeto202526/flagsinglaterra.png", "imagens_projeto202526/flagsjapao.png"
  ]

flagFilesBracket :: [FilePath]
flagFilesBracket =
  [ "imagens_projeto202526/flagsportugalbracket.png", "imagens_projeto202526/flagsbrasilbracket.png", "imagens_projeto202526/flagsargentinabracket.png", "imagens_projeto202526/flagsfrancabracket.png"
  , "imagens_projeto202526/flagsalemanhabracket.png", "imagens_projeto202526/flagsespanhabracket.png", "imagens_projeto202526/flagsinglaterrabracket.png", "imagens_projeto202526/flagsjapaobracket.png"
  ]

countryNames :: [String]
countryNames =
  [ "Portugal", "Brasil", "Argentina", "Franca"
  , "Alemanha", "Espanha", "Inglaterra", "Japao"
  ]

main :: IO ()
main = do
  -- imagens do jogo
  mTitle         <- loadJuicyPNG "imagens_projeto202526/simbolo.png"

  -- flags
  mFlagsMenu     <- mapM loadJuicyPNG flagFilesMenu
  mFlagsBracket  <- mapM loadJuicyPNG flagFilesBracket

  -- imagens das minhocas
  let wormFiles =
        [ "imagens_projeto202526/wormsportugal.png"
        , "imagens_projeto202526/wormsbrasil.png"
        , "imagens_projeto202526/wormsargentina.png"
        , "imagens_projeto202526/wormsfranca.png"
        , "imagens_projeto202526/wormsalemanha.png"
        , "imagens_projeto202526/wormsespanha.png"
        , "imagens_projeto202526/wormsinglaterra.png"
        , "imagens_projeto202526/wormsjapao.png"
        ]
  mWormsMaybe <- mapM loadJuicyPNG wormFiles

  -- imagens de vitória
  mVictoryPT <- loadJuicyPNG "imagens_projeto202526/fundovitoriaportugal.png"
  mVictoryBR <- loadJuicyPNG "imagens_projeto202526/fundovitoriabrasil.png"

  -- verifica se alguma imagem falhou a carregar
  let missingWorms = [f | (f, Nothing) <- zip wormFiles mWormsMaybe]
      missingFlagsMenu = [f | (f, Nothing) <- zip flagFilesMenu mFlagsMenu]
      missingFlagsBracket = [f | (f, Nothing) <- zip flagFilesBracket mFlagsBracket]
      missingTitle = if mTitle == Nothing then ["imagens_projeto202526/simbolo.png"] else []
      missingVictoryFiles = [ "imagens_projeto202526/fundovitoriaportugal.png" | mVictoryPT == Nothing ] ++ [ "imagens_projeto202526/fundovitoriabrasil.png" | mVictoryBR == Nothing ]
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

      mBarril        <- loadJuicyPNG "imagens_projeto202526/barrilnormal.png" >>= ensure "imagens_projeto202526/barrilnormal.png"
      mBarrilExplode <- loadJuicyPNG "imagens_projeto202526/barrilexplodir.png" >>= ensure "imagens_projeto202526/barrilexplodir.png"
      mMina          <- loadJuicyPNG "imagens_projeto202526/minanormal.png" >>= ensure "imagens_projeto202526/minanormal.png"
      mJetpack       <- loadJuicyPNG "imagens_projeto202526/jetpacknormal.png" >>= ensure "imagens_projeto202526/jetpacknormal.png"
      mDinamite      <- loadJuicyPNG "imagens_projeto202526/dinamitenormal.png" >>= ensure "imagens_projeto202526/dinamitenormal.png"
      mBazuca        <- loadJuicyPNG "imagens_projeto202526/bazucanormal.png" >>= ensure "imagens_projeto202526/bazucanormal.png"
      mEscavadora    <- loadJuicyPNG "imagens_projeto202526/escavadoranormal.png" >>= ensure "imagens_projeto202526/escavadoranormal.png"
      mMenuBg        <- loadJuicyPNG "imagens_projeto202526/fundomenu.png" >>= ensure "imagens_projeto202526/fundomenu.png"
      mBracketBg     <- loadJuicyPNG "imagens_projeto202526/fundobracket.png" >>= ensure "imagens_projeto202526/fundobracket.png"
      mGameBg        <- loadJuicyPNG "imagens_projeto202526/fundojogo.png" >>= ensure "imagens_projeto202526/fundojogo.png"

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










































