module Desenhar where

import Graphics.Gloss

import Worms

-- | Função que desenha o estado do jogo no Gloss.
desenha :: Worms -> Picture
desenha _ = Translate (-450) 0 $ Scale 0.5 0.5 $ Text "Welcome to Worms!"
