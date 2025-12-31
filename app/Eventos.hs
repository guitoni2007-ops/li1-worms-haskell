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
import Worms (Inventario(..))  -- importa o construtor do inventário para pattern-match/construct

import qualified Tarefa1 as T1
import qualified Tarefa2 as T2
import qualified Labs2025 as L25
import Tarefa0_geral (movePosicao, ePosicaoMatrizValida)
import Tarefa0_2025 (ePosicaoEstadoLivre)
import Data.List (partition, find)

-- | Entrada principal de eventos
reageEventos :: Event -> W.Worms -> W.Worms

-- Helpers para aceder/actualizar minhoca por índice
getMinhocaIdx :: Int -> L25.Estado -> Maybe L25.Minhoca
getMinhocaIdx idx est =
  let ms = L25.minhocasEstado est
  in if idx < 0 || idx >= length ms then Nothing else Just (ms !! idx)

updateMinhocaIdx :: Int -> L25.Minhoca -> L25.Estado -> L25.Estado
updateMinhocaIdx idx newM est =
  let ms = L25.minhocasEstado est
      (before, after) = splitAt idx ms
  in case after of
       []     -> est
       (_:xs) -> est { L25.minhocasEstado = before ++ (newM : xs) }

-- Clique do rato (Down)
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx,my)) s
  -- Ao clicar Tournament no MainMenu: vai direto para a bracket (sem escolha de país)
  | W.menu s == W.MainMenu && isOverMainButton (mx,my) 0 =
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
           }

  -- Play no ecrã da bracket: inicia o próximo match não jogado
  | W.menu s == W.Game && W.showWhite s && isOverRect (mx,my) (0, playY) (playW, playH) =
      case W.bracket s of
        Nothing -> s { W.hoverPlay = False }
        Just b ->
          let firstRound = if null (W.rounds b) then [] else head (W.rounds b)
              -- encontra o primeiro match não jogado (a /= b significa não jogado)
              nextMatch = find (\(W.Match x y) -> x /= y) firstRound
          in case nextMatch of
               Just m ->
                 let est = W.criaEstadoForMatch m
                     -- encontra primeiro índice de minhoca com posição (Just)
                     firstAliveIdx = case [ i | (i, mm) <- zip [0..] (L25.minhocasEstado est), L25.posicaoMinhoca mm /= Nothing ] of
                                      (i:_) -> i
                                      []    -> 0
                 in if T1.validaEstado est
                      then s { W.menu = W.Game
                             , W.showWhite = True
                             , W.hoverMain = -1
                             , W.hoverArrow = -1
                             , W.hoverFlag = False
                             , W.hoverPlay = False
                             , W.estadoJogo = Just est
                             , W.pendingInputs = []
                             , W.tickAcc = 0.0
                             , W.currentTurn = firstAliveIdx
                             , W.turnTicksLeft = W.turnDuration s
                             }
                      else s { W.hoverPlay = False }
               Nothing ->
                 let est = W.criaEstadoInicial
                 in if T1.validaEstado est
                      then s { W.estadoJogo = Just est, W.hoverPlay = False }
                      else s { W.hoverPlay = False }

  -- MainMenu: Definições (middle)
  | W.menu s == W.MainMenu && isOverMainButton (mx,my) 1 =
      s { W.hoverMain = -1, W.hoverPlay = False }

  -- MainMenu: Exit (bottom)
  | W.menu s == W.MainMenu && isOverMainButton (mx,my) 2 =
      error "Exit"

  -- CountrySelect: setas (click)
  | W.menu s == W.CountrySelect && isOverRect (mx,my) (leftArrowX, arrowY) (arrowW, arrowH) =
      s { W.countryIndex = (W.countryIndex s - 1) `mod` 8 }

  | W.menu s == W.CountrySelect && isOverRect (mx,my) (rightArrowX, arrowY) (arrowW, arrowH) =
      s { W.countryIndex = (W.countryIndex s + 1) `mod` 8 }

  | otherwise = s { W.hoverPlay = False }

-- Movimento do rato (hover)
reageEventos (EventMotion (mx,my)) s =
  let sScale = if W.tournament s then sTournament else sNormal
      flagBoxW = (refW + outerMargin) * sScale
      flagBoxH = (refH + outerMargin) * sScale
      flagCenterY = cyBase + 20
      overPlay = isOverRect (mx,my) (0, playY) (playW, playH)
      overLeftArrow = isOverRect (mx,my) (leftArrowX, arrowY) (arrowW, arrowH)
      overRightArrow = isOverRect (mx,my) (rightArrowX, arrowY) (arrowW, arrowH)
      overFlagBox = isOverRect (mx,my) (0, flagCenterY) (flagBoxW, flagBoxH)
      mainIdx = whichMainButton (mx,my)
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
         | W.showWhite s && overPlay -> s { W.hoverPlay = True, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | W.showWhite s             -> s { W.hoverPlay = False, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }
         | otherwise                 -> s { W.hoverPlay = False, W.hoverArrow = -1, W.hoverMain = -1, W.hoverFlag = False }

-- Teclas: bufferiza inputs e trata teclas de colocar/activar objetos
reageEventos (EventKey key Down _ _) s =
  case key of
    Char ' ' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
              est' = T2.efetuaJogada idx (L25.Dispara L25.Bazuca L25.Norte) est
          in if est' /= est then s { W.estadoJogo = Just est' } else s

    Char 'n' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just m ->
                 case L25.posicaoMinhoca m of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         inv = W.sharedInventory s
                         -- pattern-match no inventário para obter campos
                         W.Inventario jet esc baz mi d = inv
                         hasDinamite = d > 0
                     in if (not occupied) && hasDinamite
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Dinamite
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                est' = est { L25.objetosEstado = disparo : L25.objetosEstado est }
                                inv' = W.Inventario jet esc baz mi (d - 1)
                            in s { W.estadoJogo = Just est', W.sharedInventory = inv' }
                          else s

    Char 'w' -> jetpackMoveUp s
    SpecialKey KeyUp -> jetpackMoveUp s

    Char 'e' -> escavadoraToggle s

    Char 'b' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just m ->
                 case L25.posicaoMinhoca m of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         inv = W.sharedInventory s
                         W.Inventario jet esc baz mi d = inv
                         hasBaz = baz > 0
                     in if (not occupied) && hasBaz
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Bazuca
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                est' = est { L25.objetosEstado = disparo : L25.objetosEstado est }
                                inv' = W.Inventario jet esc (baz - 1) mi d
                            in s { W.estadoJogo = Just est', W.sharedInventory = inv' }
                          else s

    Char 'm' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just m ->
                 case L25.posicaoMinhoca m of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                         inv = W.sharedInventory s
                         W.Inventario jet esc baz mi d = inv
                         hasMina = mi > 0
                     in if (not occupied) && hasMina
                          then
                            let disparo = L25.Disparo { L25.posicaoDisparo = pos
                                                      , L25.direcaoDisparo = L25.Norte
                                                      , L25.tipoDisparo = L25.Mina
                                                      , L25.tempoDisparo = Nothing
                                                      , L25.donoDisparo = idx
                                                      }
                                est' = est { L25.objetosEstado = disparo : L25.objetosEstado est }
                                inv' = W.Inventario jet esc baz (mi - 1) d
                            in s { W.estadoJogo = Just est', W.sharedInventory = inv' }
                          else s

    Char 'v' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let idx = W.currentTurn s
          in case getMinhocaIdx idx est of
               Nothing -> s
               Just m ->
                 case L25.posicaoMinhoca m of
                   Nothing -> s
                   Just pos ->
                     let occupied = objetoAt pos est
                     in if not occupied
                          then
                            let barril = L25.Barril { L25.posicaoBarril = pos, L25.explodeBarril = False }
                                est' = est { L25.objetosEstado = barril : L25.objetosEstado est }
                            in s { W.estadoJogo = Just est' }
                          else s

    _ ->
      case keyToInput key of
        Just i -> s { W.pendingInputs = W.pendingInputs s ++ [i] }
        Nothing -> s

reageEventos _ s = s

-- Jetpack: move up + cria/actualiza Jetpack com tempo = 1
jetpackMoveUp :: W.Worms -> W.Worms
jetpackMoveUp s =
  case W.estadoJogo s of
    Nothing -> s
    Just est ->
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
                        let objsNoJet = filter (not . isJetpack) (L25.objetosEstado est)
                            disparo = L25.Disparo { L25.posicaoDisparo = newPos
                                                  , L25.direcaoDisparo = L25.Norte
                                                  , L25.tipoDisparo = L25.Jetpack
                                                  , L25.tempoDisparo = Just 1
                                                  , L25.donoDisparo = idx
                                                  }
                            m' = m { L25.posicaoMinhoca = Nothing }
                            est' = updateMinhocaIdx idx m' (est { L25.objetosEstado = disparo : objsNoJet })
                        in s { W.estadoJogo = Just est' }

-- Escavadora toggle usando inventário partilhado
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
                     inv = W.sharedInventory s
                     W.Inventario jet esc baz mi d = inv
                     hasEsc = esc > 0
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
                            est' = updateMinhocaIdx idx m' (est { L25.objetosEstado = disparo : L25.objetosEstado est })
                            inv' = W.Inventario jet (esc - 1) baz mi d
                        in s { W.estadoJogo = Just est', W.sharedInventory = inv' }
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

-- Toggle Jetpack (desativar): procura Disparo Jetpack e restaura minhoca
toggleJetpack :: W.Worms -> W.Worms
toggleJetpack s =
  case W.estadoJogo s of
    Nothing -> s
    Just est ->
      let (jets, _) = partition isJetpack (L25.objetosEstado est)
      in case jets of
           [] -> s
           (jet:_) ->
             let pos = L25.posicaoDisparo jet
                 objs' = filter (not . isSameObj jet) (L25.objetosEstado est)
                 (before, after) = break (\x -> L25.posicaoMinhoca x == Nothing) (L25.minhocasEstado est)
             in case after of
                  [] -> s
                  (mm:mrest) ->
                    let mm' = mm { L25.posicaoMinhoca = Just pos }
                        minh' = before ++ (mm' : mrest)
                        est' = est { L25.objetosEstado = objs', L25.minhocasEstado = minh' }
                    in s { W.estadoJogo = Just est' }

-- Helpers para identificar objetos
isJetpack :: L25.Objeto -> Bool
isJetpack L25.Disparo { L25.tipoDisparo = L25.Jetpack } = True
isJetpack _ = False

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

objetoAt :: (Int,Int) -> L25.Estado -> Bool
objetoAt pos est =
  any (matchPos pos) (L25.objetosEstado est)
  where
    matchPos p o =
      case o of
        L25.Barril { L25.posicaoBarril = pb } -> pb == p
        L25.Disparo { L25.posicaoDisparo = pd } -> pd == p

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

-- Constantes e helpers de UI (mantive os teus valores)
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

mainButtonW, mainButtonH :: Float
mainButtonW = 360
mainButtonH = 50

mainButtonYs :: [Float]
mainButtonYs = [170, 90, 10]

isOverRect :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
isOverRect (mx,my) (cx,cy) (w,h) =
  let halfW = w / 2
      halfH = h / 2
  in mx >= cx - halfW && mx <= cx + halfW && my >= cy - halfH && my <= cy + halfH

isOverFlag :: Float -> Float -> W.Worms -> Bool
isOverFlag mx my s =
  let sScale = if W.tournament s then sTournament else sNormal
      boxW = (refW + outerMargin) * sScale
      boxH = (refH + outerMargin) * sScale
      cx = 0
      cy = cyBase + 20
  in isOverRect (mx,my) (cx,cy) (boxW, boxH)

isOverMainButton :: (Float,Float) -> Int -> Bool
isOverMainButton (mx,my) idx =
  case idx of
    0 -> isOverRect (mx,my) (0, mainButtonYs !! 0) (mainButtonW, mainButtonH)
    1 -> isOverRect (mx,my) (0, mainButtonYs !! 1) (mainButtonW, mainButtonH)
    2 -> isOverRect (mx,my) (0, mainButtonYs !! 2) (mainButtonW, mainButtonH)
    _ -> False

whichMainButton :: (Float,Float) -> Int
whichMainButton (mx,my) =
  case filter (\(i,y) -> isOverRect (mx,my) (0,y) (mainButtonW, mainButtonH)) (zip [0..] mainButtonYs) of
    ((i,_):_) -> i
    []        -> -1








