module Eventos
  ( reageEventos
  ) where

import Graphics.Gloss.Interface.Pure.Game
  ( Event(..)
  , Key(..)
  , SpecialKey(..)
  , KeyState(..)
  , MouseButton(..)
  )

import qualified Worms as W

import qualified Tarefa1 as T1
import qualified Tarefa2 as T2
import qualified Labs2025 as L25
import Tarefa0_geral (movePosicao, ePosicaoMatrizValida)
import Tarefa0_2025 (ePosicaoEstadoLivre)
import Data.List (partition, find)

-- | Entrada principal de eventos do Gloss. Gere cliques, movimento do rato e teclas.
reageEventos :: Event -> W.Worms -> W.Worms

-- ---------------------------------------------------------------------
-- Helpers de manipulação de Minhocas
-- ---------------------------------------------------------------------

-- | Obtém uma minhoca específica pelo seu índice na lista do estado.
getMinhocaIdx :: Int -> L25.Estado -> Maybe L25.Minhoca
getMinhocaIdx idx est =
  let ms = L25.minhocasEstado est
  in if idx < 0 || idx >= length ms then Nothing else Just (ms !! idx)

-- | Atualiza os dados de uma minhoca num determinado índice do estado.
updateMinhocaIdx :: Int -> L25.Minhoca -> L25.Estado -> L25.Estado
updateMinhocaIdx idx newM est =
  let ms = L25.minhocasEstado est
      (before, after) = splitAt idx ms
  in case after of
       []     -> est
       (_:xs) -> est { L25.minhocasEstado = before ++ (newM : xs) }

-- | Reduz em 1 a munição de bazuca da minhoca.
decrBazuca :: L25.Minhoca -> L25.Minhoca
decrBazuca m = m { L25.bazucaMinhoca = max 0 (L25.bazucaMinhoca m - 1) }

-- | Reduz em 1 a munição de minas da minhoca.
decrMina :: L25.Minhoca -> L25.Minhoca
decrMina m = m { L25.minaMinhoca = max 0 (L25.minaMinhoca m - 1) }

-- | Reduz em 1 a munição de dinamite da minhoca.
decrDinamite :: L25.Minhoca -> L25.Minhoca
decrDinamite m = m { L25.dinamiteMinhoca = max 0 (L25.dinamiteMinhoca m - 1) }

-- | Reduz em 1 o uso disponível da escavadora.
decrEscavadoraInv :: L25.Minhoca -> L25.Minhoca
decrEscavadoraInv m = m { L25.escavadoraMinhoca = max 0 (L25.escavadoraMinhoca m - 1) }

-- ---------------------------------------------------------------------
-- Constantes de UI para Interação
-- ---------------------------------------------------------------------

refW, refH, outerMargin :: Float
refW = 276
refH = 183
outerMargin = 6

sTournament, sNormal :: Float
sTournament = 1.10
sNormal     = 0.80

cyBase :: Float
cyBase = -20

leftArrowX, rightArrowX, arrowY :: Float
leftArrowX = -420
rightArrowX = 420
arrowY = -20

arrowScale :: Float
arrowScale = 1.4

arrowW, arrowH :: Float
arrowW = 40 * arrowScale
arrowH = 60 * arrowScale

playW, playH, playY :: Float
playW = 220
playH = 64
playY = -260

backW, backH, backY :: Float
backW = 160
backH = 48
backY = -380

statsY :: Float
statsY = backY + 70

replayW, replayH, replayY :: Float
replayW = 220
replayH = 64
replayY = -120

mainButtonW, mainButtonH :: Float
mainButtonW = 360
mainButtonH = 50

mainButtonYs :: [Float]
mainButtonYs = [170, 90, 10]

-- | Verifica se um ponto (rato) está dentro de um retângulo.
isOverRect :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
isOverRect (mx,my) (cx,cy) (w,h) =
  let halfW = w / 2
      halfH = h / 2
  in mx >= cx - halfW && mx <= cx + halfW && my >= cy - halfH && my <= cy + halfH

-- | Deteta se o rato está sobre um dos botões do menu principal.
isOverMainButton :: (Float,Float) -> Int -> Bool
isOverMainButton (mx,my) idx =
  let n = length mainButtonYs
  in if idx < 0 || idx >= n
       then False
       else
         let cy = mainButtonYs !! idx
         in isOverRect (mx,my) (0, cy) (mainButtonW, mainButtonH)

-- | Identifica qual o índice do botão do menu principal sob o rato.
whichMainButton :: (Float,Float) -> Int
whichMainButton (mx,my) =
  case filter (\(i,y) -> isOverRect (mx,my) (0,y) (mainButtonW, mainButtonH)) (zip [0..] mainButtonYs) of
    ((i,_):_) -> i
    []        -> -1

-- ---------------------------------------------------------------------
-- Predicados de Objetos
-- ---------------------------------------------------------------------

isJetpack :: L25.Objeto -> Bool
isJetpack L25.Disparo { L25.tipoDisparo = L25.Jetpack } = True
isJetpack _ = False

isJetpackOf :: Int -> L25.Objeto -> Bool
isJetpackOf idx o =
  case o of
    L25.Disparo { L25.tipoDisparo = L25.Jetpack, L25.donoDisparo = d } -> d == idx
    _ -> False

isEscavadora :: L25.Objeto -> Bool
isEscavadora L25.Disparo { L25.tipoDisparo = L25.Escavadora } = True
isEscavadora _ = False

isSameObj :: L25.Objeto -> L25.Objeto -> Bool
isSameObj a b =
  case (a,b) of
    (L25.Disparo { L25.posicaoDisparo = pa, L25.tipoDisparo = ta },
     L25.Disparo { L25.posicaoDisparo = pb, L25.tipoDisparo = tb }) -> pa == pb && ta == tb
    (L25.Barril { L25.posicaoBarril = pa }, L25.Barril { L25.posicaoBarril = pb }) -> pa == pb
    _ -> False

-- | Verifica se existe algum objeto (barril/disparo) numa posição.
objetoAt :: (Int,Int) -> L25.Estado -> Bool
objetoAt pos est =
  any (matchPos pos) (L25.objetosEstado est)
  where
    matchPos p o =
      case o of
        L25.Barril { L25.posicaoBarril = pb } -> pb == p
        L25.Disparo { L25.posicaoDisparo = pd } -> pd == p

-- ---------------------------------------------------------------------
-- Lógica de Cliques do Rato (Down)
-- ---------------------------------------------------------------------

reageEventos (EventKey (MouseButton LeftButton) Down _ (mx,my)) s =
  let coordsForMain = (mx, my)
  in
  -- Botão Quick Play (Índice 0)
  if W.menu s == W.MainMenu && isOverMainButton coordsForMain 0
    then
      let est = W.criaEstadoInicial
          firstAliveIdx = case [ i | (i, mm) <- zip [0..] (L25.minhocasEstado est), L25.posicaoMinhoca mm /= Nothing ] of
                           (i:_) -> i
                           []    -> 0
      in if T1.validaEstado est
           then s { W.menu = W.Game
                  , W.showWhite = True
                  , W.estadoJogo = Just est
                  , W.pendingInputs = []
                  , W.tickAcc = 0.0
                  , W.currentTurn = firstAliveIdx
                  , W.turnTicksLeft = W.turnDuration s
                  , W.currentMatch = Nothing
                  , W.lastWinner = Nothing
                  , W.lastMatchInitial = Just est
                  , W.lastMatchFinal = Nothing
                  , W.showStatistics = False
                  , W.hoverMain = -1
                  , W.hoverPlay = False
                  , W.hoverFlag = False
                  , W.tournament = False
                  , W.bracket = Nothing
                  }
           else s
  else
  -- Botão Tournament (Índice 1)
  if W.menu s == W.MainMenu && isOverMainButton coordsForMain 1
    then
      let countryNames =
            [ "Portugal", "Brasil", "Argentina", "Franca"
            , "Alemanha", "Espanha", "Inglaterra", "Japao"
            ]
          b = W.seedBracketFromList countryNames
      in s { W.menu = W.Game
           , W.tournament = True
           , W.bracket = Just b
           , W.showWhite = True
           , W.hoverMain = -1
           , W.hoverFlag = False
           , W.hoverPlay = False
           , W.estadoJogo = Nothing
           , W.currentMatch = Nothing
           , W.lastWinner = Nothing
           , W.lastMatchInitial = Nothing
           , W.lastMatchFinal = Nothing
           , W.showStatistics = False
           }
  else
  -- Botão Exit (Índice 2)
  if W.menu s == W.MainMenu && isOverMainButton coordsForMain 2
    then error "Sair"
  else
  -- Botão de Estatísticas no ecrã final
  if W.lastWinner s /= Nothing && isOverRect (mx,my) (0, statsY) (backW, backH)
    then s { W.showStatistics = True
           , W.hoverPlay = False
           , W.hoverFlag = False
           , W.hoverMain = -1
           }
  else
  -- Botão Back para o Menu Principal
  if W.lastWinner s /= Nothing && isOverRect (mx,my) (0, backY) (backW, backH)
    then s { W.menu = W.MainMenu
           , W.tournament = False
           , W.bracket = Nothing
           , W.showWhite = False
           , W.estadoJogo = Nothing
           , W.lastWinner = Nothing
           , W.lastMatchInitial = Nothing
           , W.lastMatchFinal = Nothing
           , W.showStatistics = False
           , W.hoverPlay = False
           , W.hoverFlag = False
           , W.hoverMain = -1
           }
    else
      -- Botão Replay na Bracket do Torneio
      if W.menu s == W.Game && W.tournament s && W.estadoJogo s == Nothing && W.lastWinner s == Nothing && isOverRect (mx,my) (0, replayY) (replayW, replayH)
        then
          let est = W.criaEstadoInicial
              firstAliveIdx = case [ i | (i, mm) <- zip [0..] (L25.minhocasEstado est), L25.posicaoMinhoca mm /= Nothing ] of
                                (i:_) -> i
                                []    -> 0
          in if T1.validaEstado est
               then s { W.estadoJogo = Just est
                      , W.showWhite = True
                      , W.pendingInputs = []
                      , W.tickAcc = 0.0
                      , W.currentTurn = firstAliveIdx
                      , W.turnTicksLeft = W.turnDuration s
                      , W.hoverMain = -1
                      , W.hoverPlay = False
                      , W.hoverFlag = False
                      , W.lastMatchInitial = Just est
                      , W.lastMatchFinal = Nothing
                      , W.showStatistics = False
                      }
               else s
      else
        -- Seleção de Países (Setas)
        if W.menu s == W.CountrySelect && isOverRect (mx,my) (leftArrowX, arrowY) (arrowW, arrowH)
          then s { W.countryIndex = (W.countryIndex s - 1) `mod` 8 }
        else if W.menu s == W.CountrySelect && isOverRect (mx,my) (rightArrowX, arrowY) (arrowW, arrowH)
          then s { W.countryIndex = (W.countryIndex s + 1) `mod` 8 }
        else
          -- Sair do ecrã de estatísticas para o menu
          if W.showStatistics s && isOverRect (mx,my) (0, backY) (backW, backH)
            then s { W.showStatistics = False
                   , W.menu = W.MainMenu
                   , W.tournament = False
                   , W.bracket = Nothing
                   , W.lastWinner = Nothing
                   , W.lastMatchInitial = Nothing
                   , W.lastMatchFinal = Nothing
                   , W.hoverMain = -1
                   }
            else s { W.hoverPlay = False }

-- ---------------------------------------------------------------------
-- Lógica de Movimento do Rato (Hover)
-- ---------------------------------------------------------------------

reageEventos (EventMotion (mx,my)) s =
  let coordsForMain = (mx, my)
      sScale = if W.tournament s then sTournament else sNormal
      flagBoxW = (refW + outerMargin) * sScale
      flagBoxH = (refH + outerMargin) * sScale
      flagCenterY = cyBase + 20
      overPlay = isOverRect (mx,my) (0, playY) (playW, playH)
      overLeftArrow = isOverRect (mx,my) (leftArrowX, arrowY) (arrowW, arrowH)
      overRightArrow = isOverRect (mx,my) (rightArrowX, arrowY) (arrowW, arrowH)
      overFlagBox = isOverRect (mx,my) (0, flagCenterY) (flagBoxW, flagBoxH)
      mainIdx = whichMainButton coordsForMain
      overBack = isOverRect (mx,my) (0, backY) (backW, backH)
      overStats = isOverRect (mx,my) (0, statsY) (backW, backH)
      overReplay = isOverRect (mx,my) (0, replayY) (replayW, replayH)
  in case W.menu s of
       W.MainMenu
         | mainIdx >= 0 -> s { W.hoverMain = mainIdx, W.hoverArrow = -1, W.hoverFlag = False, W.hoverPlay = False }
         | otherwise    -> s { W.hoverMain = -1, W.hoverArrow = -1, W.hoverFlag = False, W.hoverPlay = False }

       W.CountrySelect
         | overLeftArrow  -> s { W.hoverArrow = 0, W.hoverMain = -1, W.hoverFlag = False, W.hoverPlay = False }
         | overRightArrow -> s { W.hoverArrow = 1, W.hoverMain = -1, W.hoverFlag = False, W.hoverPlay = False }
         | overFlagBox    -> s { W.hoverFlag = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverPlay = False }
         | otherwise      -> s { W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False, W.hoverPlay = False }

       W.Game
         | W.tournament s && W.estadoJogo s == Nothing && W.lastWinner s == Nothing && overReplay ->
             s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.tournament s && W.estadoJogo s == Nothing && W.lastWinner s == Nothing && overBack ->
             s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.lastWinner s /= Nothing && overStats ->
             s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.lastWinner s /= Nothing && overBack ->
             s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.showStatistics s && overBack ->
             s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.showWhite s && overPlay -> s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.showWhite s             -> s { W.hoverPlay = False, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | otherwise                 -> s { W.hoverPlay = False, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }

-- ---------------------------------------------------------------------
-- Lógica de Teclado (Inputs do Jogo)
-- ---------------------------------------------------------------------

reageEventos (EventKey key Down _ _) s =
  case key of
    -- Disparar Bazuca (Espaço)
    Char ' ' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
              est' = T2.efetuaJogada idx (L25.Dispara L25.Bazuca L25.Norte) est
          in if est' /= est then s { W.estadoJogo = Just est' } else s

    -- Colocar Dinamite (N)
    Char 'n' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just mm ->
                 case L25.posicaoMinhoca mm of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         hasDinamite = L25.dinamiteMinhoca mm > 0
                     in if (not occupied) && hasDinamite
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Dinamite
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                mm' = decrDinamite mm
                                est' = updateMinhocaIdx idx mm' (est { L25.objetosEstado = disparo : L25.objetosEstado est })
                            in s { W.estadoJogo = Just est' }
                          else s

    -- Controlar Jetpack (W / Up)
    Char 'w' -> jetpackMoveUp s
    SpecialKey KeyUp -> jetpackMoveUp s

    -- Ativar/Desativar Escavadora (E)
    Char 'e' -> escavadoraToggle s

    -- Colocar Bazuca estática (B)
    Char 'b' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just mm ->
                 case L25.posicaoMinhoca mm of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         hasBaz = L25.bazucaMinhoca mm > 0
                     in if (not occupied) && hasBaz
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Bazuca
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                mm' = decrBazuca mm
                                est' = updateMinhocaIdx idx mm' (est { L25.objetosEstado = disparo : L25.objetosEstado est })
                            in s { W.estadoJogo = Just est' }
                          else s

    -- Colocar Mina (M)
    Char 'm' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just mm ->
                 case L25.posicaoMinhoca mm of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         hasMina = L25.minaMinhoca mm > 0
                     in if (not occupied) && hasMina
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Mina
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                mm' = decrMina mm
                                est' = updateMinhocaIdx idx mm' (est { L25.objetosEstado = disparo : L25.objetosEstado est })
                            in s { W.estadoJogo = Just est' }
                          else s

    -- Colocar Barril de teste (V)
    Char 'v' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just mm ->
                 case L25.posicaoMinhoca mm of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                     in if not occupied
                          then
                            let barril = L25.Barril { L25.posicaoBarril = pos, L25.explodeBarril = False }
                                est' = est { L25.objetosEstado = barril : L25.objetosEstado est }
                            in s { W.estadoJogo = Just est' }
                          else s

    -- Outras teclas de movimento e ação (buffer de inputs)
    _ ->
      case keyToInput key of
        Just i -> s { W.pendingInputs = W.pendingInputs s ++ [i] }
        Nothing -> s

reageEventos _ s = s

-- ---------------------------------------------------------------------
-- Funções de Lógica Especial de Movimento
-- ---------------------------------------------------------------------

jetpackDuration :: Int
jetpackDuration = 4

minJetpackTicks :: Int
minJetpackTicks = 2

-- | Move a minhoca para cima usando o Jetpack e consome tempo de voo.
jetpackMoveUp :: W.Worms -> W.Worms
jetpackMoveUp s =
  case W.estadoJogo s of
    Nothing -> s
    Just est ->
      if W.turnTicksLeft s <= minJetpackTicks
        then s
        else
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just m ->
                 case L25.posicaoMinhoca m of
                   Nothing -> toggleJetpack s
                   Just pos ->
                     let mapa = L25.mapaEstado est
                         newPos = movePosicao L25.Norte pos
                         canMove = ePosicaoMatrizValida newPos mapa && ePosicaoEstadoLivre newPos est
                     in if not canMove
                          then s
                          else
                            let objsNoJet = filter (not . isJetpackOf idx) (L25.objetosEstado est)
                                disparo = L25.Disparo { L25.posicaoDisparo = newPos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Jetpack
                                                      , L25.tempoDisparo = Just jetpackDuration
                                                      , L25.donoDisparo = idx
                                                      }
                                m' = m { L25.posicaoMinhoca = Nothing }
                                est' = updateMinhocaIdx idx m' (est { L25.objetosEstado = disparo : objsNoJet })
                            in s { W.estadoJogo = Just est' }

-- | Alterna entre o estado de escavação e o estado normal da minhoca.
escavadoraToggle :: W.Worms -> W.Worms
escavadoraToggle s =
  case W.estadoJogo s of
    Nothing -> s
    Just est ->
      let idx = W.currentTurn s
      in case getMinhocaIdx idx est of
           Nothing -> s
           Just m ->
             case L25.posicaoMinhoca m of
               Just pos ->
                 let occupied = objetoAt pos est
                     hasEsc = L25.escavadoraMinhoca m > 0
                 in if occupied || not hasEsc
                      then s
                      else
                        let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                  , L25.direcaoDisparo = L25.Norte
                                                  , L25.tipoDisparo = L25.Escavadora
                                                  , L25.tempoDisparo = Nothing
                                                  , L25.donoDisparo = idx
                                                  }
                            m' = m { L25.posicaoMinhoca = Nothing }
                            mInv' = decrEscavadoraInv m
                            est' = updateMinhocaIdx idx mInv' (est { L25.objetosEstado = disparo : L25.objetosEstado est })
                        in s { W.estadoJogo = Just est' }
               Nothing ->
                 let (escs, _) = partition isEscavadora (L25.objetosEstado est)
                 in case escs of
                      [] -> s
                      (esc:_) ->
                        let pos = L25.posicaoDisparo esc
                            objs' = filter (not . isSameObj esc) (L25.objetosEstado est)
                            (before, after) = break (\x -> L25.posicaoMinhoca x == Nothing) (L25.minhocasEstado est)
                        in case after of
                             [] -> s
                             (mm:mrest) ->
                               let mm' = mm { L25.posicaoMinhoca = Just pos }
                                   minh' = before ++ (mm' : mrest)
                                   est' = est { L25.objetosEstado = objs', L25.minhocasEstado = minh' }
                               in s { W.estadoJogo = Just est' }

-- | Desativa o Jetpack e coloca a minhoca de volta no mapa.
toggleJetpack :: W.Worms -> W.Worms
toggleJetpack s =
  case W.estadoJogo s of
    Nothing -> s
    Just est ->
      let (jets, others) = partition isJetpack (L25.objetosEstado est)
      in case jets of
           [] -> s
           (jet:_) ->
             let pos = L25.posicaoDisparo jet
                 ownerIdx = L25.donoDisparo jet
                 objs' = filter (not . isSameObj jet) (L25.objetosEstado est)
                 minhList = L25.minhocasEstado est
                 minhocasRestored =
                   if ownerIdx >= 0 && ownerIdx < length minhList
                     then
                       let (before, after) = splitAt ownerIdx minhList
                       in case after of
                            [] -> minhList
                            (mm:mrest) ->
                              let mm' = mm { L25.posicaoMinhoca = Just pos }
                              in before ++ (mm' : mrest)
                     else
                       let (before, after) = break (\x -> L25.posicaoMinhoca x == Nothing) minhList
                       in case after of
                            [] -> minhList
                            (mm:mrest) ->
                              let mm' = mm { L25.posicaoMinhoca = Just pos }
                              in before ++ (mm' : mrest)
                 est' = est { L25.objetosEstado = objs', L25.minhocasEstado = minhocasRestored }
             in s { W.estadoJogo = Just est' }

-- | Mapeia teclas Gloss para o tipo de Input interno.
keyToInput :: Key -> Maybe W.Input
keyToInput (SpecialKey KeyUp)    = Just W.IUp
keyToInput (SpecialKey KeyDown)  = Just W.IDown
keyToInput (SpecialKey KeyLeft)  = Just W.ILeft
keyToInput (SpecialKey KeyRight) = Just W.IRight
keyToInput (Char 'w')            = Just W.IUp
keyToInput (Char 's')            = Just W.IDown
keyToInput (Char 'a')            = Just W.ILeft
keyToInput (Char 'd')            = Just W.IRight
keyToInput (Char ' ')            = Just W.IFire
keyToInput _                     = Nothing













