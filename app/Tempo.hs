module Tempo (reageTempo) where

import qualified Worms as W
import qualified Labs2025 as L25
import qualified Tarefa2 as T2
import qualified Tarefa3 as T3
import Data.List (foldl')

type Segundos = Float

-- Mantemos 60 FPS no render; reduzimos ticks por segundo para aumentar duração dos ticks.
tickRate :: Int
tickRate = 2    -- exemplo: 6 ticks por segundo (ajusta se quiseres mais/menos)

tickDuration :: Float
tickDuration = 1 / fromIntegral tickRate

-- reageTempo acumula dt e processa 0..n ticks por frame
reageTempo :: Segundos -> W.Worms -> W.Worms
reageTempo dt s =
  let acc0 = W.tickAcc s + dt
      (nTicks, accRem) = properDiv acc0 tickDuration
  in processNTicks nTicks accRem s

properDiv :: Float -> Float -> (Int, Float)
properDiv acc dur =
  let n = floor (acc / dur)
      remt = acc - fromIntegral n * dur
  in (n, remt)

-- Converte um Input para uma Jogada quando aplicável (IFire -> Dispara).
inputToJogada :: W.Input -> Maybe L25.Jogada
inputToJogada W.IFire = Just (L25.Dispara L25.Bazuca L25.Norte)  -- exemplo; adapta se quiseres
inputToJogada _       = Nothing

-- Aplica a lista de inputs ao Estado (usando Tarefa2.efetuaJogada para jogadas que alteram o estado,
-- e applyInputsToEstado para movimentos). Usa a minhoca índice 0 por padrão.
aplicaPendingInputs :: [W.Input] -> L25.Estado -> L25.Estado
aplicaPendingInputs inputs est = foldl' aplica est inputs
  where
    aplica e inp =
      case inputToJogada inp of
        Just jog -> T2.efetuaJogada 0 jog e
        Nothing  -> W.applyInputsToEstado [inp] e

-- Processa n ticks sequenciais; em cada tick:
-- 1) consome pendingInputs e aplica ao Estado
-- 2) chama Tarefa3.avancaEstado
-- 3) commit do estado retornado por avancaEstado
processNTicks :: Int -> Float -> W.Worms -> W.Worms
processNTicks 0 acc s = s { W.tickAcc = acc }
processNTicks n acc s =
  case W.estadoJogo s of
    Nothing -> s { W.tickAcc = acc }  -- nada a processar
    Just est ->
      let inputsThisTick = W.pendingInputs s
          sCleared = s { W.pendingInputs = [] }  -- limpa buffer antes de aplicar
          -- aplica inputs bufferizados ao estado
          estAfterInputs = aplicaPendingInputs inputsThisTick est
          -- avança o estado um tick
          estAdvanced = T3.avancaEstado estAfterInputs
          s' = sCleared { W.estadoJogo = Just estAdvanced }
      in processNTicks (n - 1) acc s'













