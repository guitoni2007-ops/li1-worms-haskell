module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Desenhar (desenha)
import Eventos (reageEventos)
import Worms (initialState, Worms)
import Tempo (reageTempo)

janela :: Display
janela = InWindow "Worms World Cup" (1280, 720) (100, 100)

fundo :: Color
fundo = greyN 0.5

fr :: Int
fr = 60

flagFiles :: [FilePath]
flagFiles =
  [ "flagsportugal.png", "flagsbrasil.png", "flagsargentina.png", "flagsfranca.png"
  , "flagsalemanha.png", "flagsespanha.png", "flagsinglaterra.png", "flagsjapao.png"
  ]

main :: IO ()
main = do
  mTitle <- loadJuicyPNG "Worms world cup.png"
  titlePic <- case mTitle of
    Just p  -> return p
    Nothing -> return $ Translate 0 0 $ Scale 0.001 0.001 $ Text "WORMS WORLD CUP"

  mFlags <- mapM loadJuicyPNG flagFiles

  let it :: Worms
      it = initialState

  play janela fundo fr it (\w -> desenha titlePic mFlags w) reageEventos reageTempo
















