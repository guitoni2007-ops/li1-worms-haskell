module Tempo (reageTempo) where

import qualified Worms as W
import qualified Labs2025 as L25
import qualified Tarefa2 as T2
import qualified Tarefa3 as T3
import Tarefa0_2025 (ePosicaoEstadoLivre)
import Tarefa0_geral (movePosicao)
import Data.List (partition, find)
import Data.Maybe (isJust, fromMaybe)

type Segundos = Float

-- | Define quantos passos de simulação ocorrem por segundo.
tickRate :: Int
tickRate = 2

-- | Tempo real correspondente a cada tick.
tickDuration :: Float
tickDuration = 1 / fromIntegral tickRate

-- | Função principal chamada pelo Gloss a cada instante de tempo.
reageTempo :: Segundos -> W.Worms -> W.Worms
reageTempo dt s =
  let acc0 = W.tickAcc s + dt
      (nTicks, accRem) = properDiv acc0 tickDuration
  in processNTicks nTicks accRem s

-- | Auxiliar para dividir o tempo acumulado em ticks inteiros.
properDiv :: Float -> Float -> (Int, Float)
properDiv acc dur =
  let n = floor (acc / dur)
      remt = acc - fromIntegral n * dur
  in (n, remt)

-- | Converte inputs específicos em jogadas da Tarefa 2.
inputToJogada :: W.Input -> Maybe L25.Jogada
inputToJogada W.IFire = Just (L25.Dispara L25.Bazuca L25.Norte)
inputToJogada _       = Nothing

-- | Processa os inputs acumulados e aplica-os ao estado do jogo.
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

-- | Remove qualquer Disparo Jetpack cujo dono seja 'idx', sem alterar minhocas.
removeJetpackObjectsForIdx :: Int -> L25.Estado -> L25.Estado
removeJetpackObjectsForIdx idx est =
  let objs = L25.objetosEstado est
      objs' = filter (not . isJetpackOf idx) objs
  in est { L25.objetosEstado = objs' }

-- | Helper para identificar Disparo Jetpack de um dono específico.
isJetpackOf :: Int -> L25.Objeto -> Bool
isJetpackOf idx o =
  case o of
    L25.Disparo { L25.tipoDisparo = L25.Jetpack, L25.donoDisparo = d } -> d == idx
    _ -> False

-- | Decrementa o campo tempoDisparo de todos os Disparo Jetpack (Just n -> Just (n-1)).
decrementJetpackTimers :: L25.Estado -> L25.Estado
decrementJetpackTimers est =
  let objs = L25.objetosEstado est
      dec o@(L25.Disparo { L25.tipoDisparo = L25.Jetpack, L25.tempoDisparo = Just t }) =
        o { L25.tempoDisparo = Just (t - 1) }
      dec o = o
  in est { L25.objetosEstado = map dec objs }

-- | Tenta restaurar minhocas para jetpacks cujo tempo chegou a 0 ou menos.
restoreExpiredJetpacks :: W.Worms -> L25.Estado -> L25.Estado
restoreExpiredJetpacks _w est =
  let objs = L25.objetosEstado est

      -- separa jets expirados dos restantes objetos
      (expired, others) = partition isExpiredJet objs

      isExpiredJet o =
        case o of
          L25.Disparo { L25.tipoDisparo = L25.Jetpack, L25.tempoDisparo = Just t } -> t <= 0
          _ -> False

      -- estado sem os jets expirados (usado para verificar posições livres)
      estNoExpired = est { L25.objetosEstado = others }

      -- util: verifica se minhoca no índice i está viva e sem posição
      minhocaPodeSerRestaurada :: Int -> L25.Estado -> Bool
      minhocaPodeSerRestaurada i e =
        if i >= 0 && i < length (L25.minhocasEstado e)
          then
            let mm = (L25.minhocasEstado e) !! i
            in case L25.vidaMinhoca mm of
                 L25.Viva _ -> L25.posicaoMinhoca mm == Nothing
                 _ -> False
          else False

      -- encontra posição livre: pos, adjacentes (N,E,S,W)
      findFreePos pos e =
        if ePosicaoEstadoLivre pos e then Just pos
        else
          let adj = [ movePosicao L25.Norte pos
                    , movePosicao L25.Este pos
                    , movePosicao L25.Sul pos
                    , movePosicao L25.Oeste pos
                    ]
          in find (`ePosicaoEstadoLivre` e) adj

      -- actualiza minhoca no índice ownerIdx para posicao = Just p
      updateMinhocaAt ownerIdx p e =
        let ms = L25.minhocasEstado e
            (before, after) = splitAt ownerIdx ms
        in case after of
             [] -> e
             (mm:rest) ->
               let mm' = mm { L25.posicaoMinhoca = Just p }
               in e { L25.minhocasEstado = before ++ (mm' : rest) }

      -- fallback: coloca a minhoca viva no primeiro slot livre
      restoreToFirstFreeSlot pos e =
        let ms = L25.minhocasEstado e
            (before, after) = break (\m -> L25.posicaoMinhoca m == Nothing && case L25.vidaMinhoca m of L25.Viva _ -> True; _ -> False) ms
        in case after of
             [] -> e
             (mm:mrest) ->
               let mm' = mm { L25.posicaoMinhoca = Just pos }
               in e { L25.minhocasEstado = before ++ (mm' : mrest) }

      -- processa um jet expirado: tenta restaurar a minhoca usando estNoExpired
      processExpired e jet =
        case jet of
          L25.Disparo { L25.posicaoDisparo = pos, L25.donoDisparo = ownerIdx } ->
            if minhocaPodeSerRestaurada ownerIdx estNoExpired
              then
                case findFreePos pos estNoExpired of
                  Just p -> updateMinhocaAt ownerIdx p e
                  Nothing -> restoreToFirstFreeSlot pos e
              else e
          _ -> e

      estAfter = foldl processExpired est expired
  in estAfter { L25.objetosEstado = others }

-- | Processa N ticks de simulação, atualizando física, timers e turnos.
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

          -- decrementa timers de jetpack
          estTimers = decrementJetpackTimers estAdvanced

          -- restaura jetpacks expirados (se houver)
          estRestored = restoreExpiredJetpacks sCleared estTimers

          -- detecta se alguma minhoca morreu (vida diferente de Viva)
          algumaMorreu = any (\m -> case L25.vidaMinhoca m of
                                     L25.Viva _ -> False
                                     _          -> True)
                             (L25.minhocasEstado estRestored)

          s' = sCleared { W.estadoJogo = Just estRestored }
      in if algumaMorreu
           then
             -- termina o jogo: remove o estado de jogo e determina o vencedor
             let -- encontra índices das minhocas vivas
                 minhList = L25.minhocasEstado estRestored
                 aliveIdxs = [ i | (i,m) <- zip [0..] minhList, case L25.vidaMinhoca m of L25.Viva _ -> True; _ -> False ]
                 winnerName =
                   case (aliveIdxs, W.currentMatch sCleared) of
                     (i:_, Just (W.Match a b)) -> Just (if i == 0 then a else b)
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
                             , W.lastMatchFinal = Just estRestored
                             }
             in sEnded
           else
             -- agora actualiza ticksLeft e possivelmente troca de turno
             let ticksLeft = W.turnTicksLeft s' - 1
             in if ticksLeft <= 0
                  then
                    let nMinh = max 1 (length (L25.minhocasEstado estRestored))
                        newIdx = (W.currentTurn s' + 1) `mod` nMinh
                        sNext = s' { W.currentTurn = newIdx
                                   , W.turnTicksLeft = W.turnDuration s'
                                   , W.pendingInputs = []
                                   }
                    in processNTicks (n - 1) acc sNext
                  else
                    let sNext = s' { W.turnTicksLeft = ticksLeft }
                    in processNTicks (n - 1) acc sNext





























