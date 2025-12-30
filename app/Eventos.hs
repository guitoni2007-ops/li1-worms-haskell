module Eventos
  ( reageEventos
  ) where

import Graphics.Gloss.Interface.Pure.Game
import qualified Worms as W
import qualified Tarefa1 as T1
import qualified Tarefa2 as T2
import qualified Labs2025 as L25

-- | Entrada principal de eventos
reageEventos :: Event -> W.Worms -> W.Worms

-- Clique do rato (Down)
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx,my)) s
  -- CountrySelect: abre bracket se o clique estiver dentro da caixa da bandeira
  | W.menu s == W.CountrySelect && isOverFlag mx my s =
      s { W.menu = W.Game
        , W.showWhite = True
        , W.hoverMain = -1
        , W.hoverArrow = -1
        , W.hoverFlag = False
        , W.hoverPlay = False
        }

  -- Play no ecrã da bracket
  | W.menu s == W.Game && W.showWhite s && isOverRect (mx,my) (0, playY) (playW, playH) =
      let est = W.criaEstadoInicial
      in s { W.menu = W.Game
           , W.showWhite = True
           , W.hoverMain = -1
           , W.hoverArrow = -1
           , W.hoverFlag = False
           , W.hoverPlay = False
           , W.estadoJogo = Just est
           , W.pendingInputs = []
           , W.tickAcc = 0.0
           }

  -- MainMenu: Tournament (top)
  | W.menu s == W.MainMenu && isOverMainButton (mx,my) 0 =
      s { W.menu = W.CountrySelect, W.tournament = True, W.hoverMain = -1, W.showWhite = False, W.hoverPlay = False }

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
      -- determina qual botão principal (0..2) está em hover, ou -1 se nenhum
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

-- Teclas: bufferiza inputs
reageEventos (EventKey key Down _ _) s =
  case key of
    Char ' ' ->
      case W.estadoJogo s of
        Nothing -> s
        Just est ->
          let est' = T2.efetuaJogada 0 (L25.Dispara L25.Bazuca L25.Norte) est
          in if est' /= est then s { W.estadoJogo = Just est' } else s
    _ ->
      case keyToInput key of
        Just i -> s { W.pendingInputs = W.pendingInputs s ++ [i] }
        Nothing -> s

reageEventos _ s = s

-- Converte Key para Input do jogo
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

-- =========================
-- Constantes (devem espelhar Desenhar.hs)
-- =========================

-- caixa de destaque das flags: refW/refH + outerMargin (estes valores vêm de Desenhar.hs)
refW, refH, outerMargin :: Float
refW = 276
refH = 183
outerMargin = 6

-- escala usada no CountrySelect (igual ao Desenhar.hs)
sTournament, sNormal :: Float
sTournament = 1.10
sNormal     = 0.80

-- posição vertical base usada em Desenhar.hs (cy = -20)
cyBase :: Float
cyBase = -20

-- setas esquerda/direita
leftArrowX, rightArrowX, arrowY :: Float
leftArrowX = -420
rightArrowX = 420
arrowY = -20

-- as setas são desenhadas com pontos x ∈ [-20,20], y ∈ [-30,30] e depois escaladas
arrowScale :: Float
arrowScale = 1.4

arrowW, arrowH :: Float
arrowW = 40 * arrowScale
arrowH = 60 * arrowScale

-- botão Play (conforme Desenhar.hs)
playW, playH, playY :: Float
playW = 220
playH = 64
playY = -260

-- botões do menu principal (conforme Desenhar.hs)
mainButtonW, mainButtonH :: Float
mainButtonW = 360
mainButtonH = 50

mainButtonYs :: [Float]
mainButtonYs = [170, 90, 10]  -- indices 0,1,2 correspondem aos 3 botões

-- =========================
-- Funções auxiliares de hit testing
-- =========================

-- Teste genérico para rectângulos axis-aligned centrados
isOverRect :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
isOverRect (mx,my) (cx,cy) (w,h) =
  let halfW = w / 2
      halfH = h / 2
  in mx >= cx - halfW && mx <= cx + halfW && my >= cy - halfH && my <= cy + halfH

-- Teste específico para a caixa da bandeira (usa as mesmas constantes que Desenhar.hs)
isOverFlag :: Float -> Float -> W.Worms -> Bool
isOverFlag mx my s =
  let sScale = if W.tournament s then sTournament else sNormal
      boxW = (refW + outerMargin) * sScale
      boxH = (refH + outerMargin) * sScale
      cx = 0
      cy = cyBase + 20
  in isOverRect (mx,my) (cx,cy) (boxW, boxH)

-- Determina se o rato está sobre qualquer botão principal; retorna True/False
isOverMainButton :: (Float,Float) -> Int -> Bool
isOverMainButton (mx,my) idx =
  case idx of
    0 -> isOverRect (mx,my) (0, mainButtonYs !! 0) (mainButtonW, mainButtonH)
    1 -> isOverRect (mx,my) (0, mainButtonYs !! 1) (mainButtonW, mainButtonH)
    2 -> isOverRect (mx,my) (0, mainButtonYs !! 2) (mainButtonW, mainButtonH)
    _ -> False

-- Retorna índice do botão principal em hover (0..2) ou -1 se nenhum
whichMainButton :: (Float,Float) -> Int
whichMainButton (mx,my) =
  case filter (\(i,y) -> isOverRect (mx,my) (0,y) (mainButtonW, mainButtonH)) (zip [0..] mainButtonYs) of
    ((i,_):_) -> i
    []        -> -1





































