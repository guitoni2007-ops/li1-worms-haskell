module Eventos where

import Graphics.Gloss.Interface.Pure.Game

import Worms

-- | Função que altera o estado do jogo no Gloss.
reageEventos :: Event -> Worms -> Worms
reageEventos _ it = it
