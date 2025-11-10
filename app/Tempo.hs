module Tempo where

import Worms

-- | Tempo em segundos.
type Segundos = Float

-- | Função que avança o tempo no estado do jogo no Gloss.
reageTempo :: Segundos -> Worms -> Worms
reageTempo _ it = it
