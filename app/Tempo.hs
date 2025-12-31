module Tempo (reageTempo) where

import qualified Worms as W
import qualified Labs2025 as L25
import qualified Tarefa2 as T2
import qualified Tarefa3 as T3

type Segundos = Float

tickRate :: Int
tickRate = 2

tickDuration :: Float
tickDuration = 1 / fromIntegral tickRate

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

inputToJogada :: W.Input -> Maybe L25.Jogada
inputToJogada W.IFire = Just (L25.Dispara L25.Bazuca L25.Norte)
inputToJogada _       = Nothing

aplicaPendingInputs :: Int -> [W.Input] -> L25.Estado -> L25.Estado
aplicaPendingInputs _ [] est = est
aplicaPendingInputs idx inputs est =
  let jogadas = map inputToJogada inputs
      mJog = case [j | Just j <- jogadas] of
               (j:_) -> Just j
               []    -> Nothing
  in case mJog of
       Just jog -> T2.efetuaJogada idx jog est
       Nothing  -> W.applyInputsToEstado idx inputs est

processNTicks :: Int -> Float -> W.Worms -> W.Worms
processNTicks 0 acc s = s { W.tickAcc = acc }
processNTicks n acc s =
  case W.estadoJogo s of
    Nothing -> s { W.tickAcc = acc }
    Just est ->
      let inputsThisTick = W.pendingInputs s
          sCleared = s { W.pendingInputs = [] }
          currentIdx = W.currentTurn sCleared
          estAfterInputs = aplicaPendingInputs currentIdx inputsThisTick est
          estAdvanced = T3.avancaEstado estAfterInputs

          -- detecta se alguma minhoca morreu (vida diferente de Viva)
          algumaMorreu = any (\m -> case L25.vidaMinhoca m of
                                     L25.Viva _ -> False
                                     _          -> True)
                             (L25.minhocasEstado estAdvanced)

          s' = sCleared { W.estadoJogo = Just estAdvanced }
      in if algumaMorreu
           then
             -- termina o jogo: remove o estado de jogo
             let sEnded = s' { W.estadoJogo = Nothing, W.tickAcc = acc }
             in sEnded
           else
             let ticksLeft = W.turnTicksLeft s' - 1
             in if ticksLeft <= 0
                  then
                    let nMinh = max 1 (length (L25.minhocasEstado estAdvanced))
                        newIdx = (W.currentTurn s' + 1) `mod` nMinh
                        sNext = s' { W.currentTurn = newIdx
                                   , W.turnTicksLeft = W.turnDuration s'
                                   , W.pendingInputs = []
                                   }
                    in processNTicks (n - 1) acc sNext
                  else
                    let sNext = s' { W.turnTicksLeft = ticksLeft }
                    in processNTicks (n - 1) acc sNext























