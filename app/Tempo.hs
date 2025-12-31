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

-- Remove qualquer Disparo Jetpack cujo dono seja 'idx', sem alterar minhocas
removeJetpackObjectsForIdx :: Int -> L25.Estado -> L25.Estado
removeJetpackObjectsForIdx idx est =
  let objs = L25.objetosEstado est
      objs' = filter (not . isJetpackOf idx) objs
  in est { L25.objetosEstado = objs' }

-- helper para identificar Disparo Jetpack de um dono específico
isJetpackOf :: Int -> L25.Objeto -> Bool
isJetpackOf idx o =
  case o of
    L25.Disparo { L25.tipoDisparo = L25.Jetpack, L25.donoDisparo = d } -> d == idx
    _ -> False

processNTicks :: Int -> Float -> W.Worms -> W.Worms
processNTicks 0 acc s = s { W.tickAcc = acc }
processNTicks n acc s =
  case W.estadoJogo s of
    Nothing -> s { W.tickAcc = acc }
    Just est ->
      let inputsThisTick = W.pendingInputs s
          sCleared = s { W.pendingInputs = [] }
          currentIdx = W.currentTurn sCleared

          -- aplica inputs antes de decidir se o turno acaba
          estAfterInputs = aplicaPendingInputs currentIdx inputsThisTick est

          -- calcula quantos ticks faltam para o fim do turno (antes de avançar)
          ticksLeftNow = W.turnTicksLeft sCleared - 1

          -- se o turno acaba neste tick, remove o Disparo Jetpack do dono ANTES de avançar o estado
          estForAdvance =
            if ticksLeftNow <= 0
              then removeJetpackObjectsForIdx currentIdx estAfterInputs
              else estAfterInputs

          -- agora avançamos o estado (física, gravidade, timers, etc.)
          estAdvanced = T3.avancaEstado estForAdvance

          -- detecta se alguma minhoca morreu (vida diferente de Viva)
          algumaMorreu = any (\m -> case L25.vidaMinhoca m of
                                     L25.Viva _ -> False
                                     _          -> True)
                             (L25.minhocasEstado estAdvanced)

          s' = sCleared { W.estadoJogo = Just estAdvanced }
      in if algumaMorreu
           then
             -- termina o jogo: remove o estado de jogo e determina o vencedor
             let -- encontra índices das minhocas vivas
                 minhList = L25.minhocasEstado estAdvanced
                 aliveIdxs = [ i | (i,m) <- zip [0..] minhList, case L25.vidaMinhoca m of L25.Viva _ -> True; _ -> False ]
                 winnerName =
                   case (aliveIdxs, W.currentMatch sCleared) of
                     -- se houver um match (tournament), usa os nomes do match
                     (i:_, Just (W.Match a b)) -> Just (if i == 0 then a else b)
                     -- caso contrário (quick play / jogo directo), tenta mapear índices 0/1 para países conhecidos
                     (i:_, Nothing) ->
                       case i of
                         0 -> Just "Portugal"
                         1 -> Just "Brasil"
                         _ -> Just ("Jogador " ++ show (i + 1))
                     _ -> Nothing
                 sEnded = s' { W.estadoJogo = Nothing
                             , W.tickAcc = acc
                             , W.lastWinner = winnerName
                             , W.currentMatch = Nothing
                             }
             in sEnded
           else
             -- agora actualiza ticksLeft e possivelmente troca de turno
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




























