module Eventos
  ( reageEventos
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Worms

-- posições e tamanhos usados em Desenhar.hs
leftArrowX, rightArrowX, arrowY :: Float
leftArrowX = -420
rightArrowX = 420
arrowY = -20

arrowW, arrowH :: Float
arrowW = 120
arrowH = 160

-- referência da "caixa" da bandeira (deve corresponder a refW/refH em Desenhar.hs)
refW, refH :: Float
refW = 520
refH = 300

-- reage a eventos do Gloss
reageEventos :: Event -> Worms -> Worms

-- clique do rato
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx,my)) s
  | menu s == MainMenu && dentro mx my 0     = s { menu = Game }
  | menu s == MainMenu && dentro mx my (-60) = s { menu = CountrySelect }
  | menu s == MainMenu && dentro mx my (-120)= error "Exit"
  | menu s == CountrySelect && dentroRect mx my leftArrowX arrowY arrowW arrowH =
      s { countryIndex = (countryIndex s - 1) `mod` 8 }
  | menu s == CountrySelect && dentroRect mx my rightArrowX arrowY arrowW arrowH =
      s { countryIndex = (countryIndex s + 1) `mod` 8 }
  | otherwise = s

-- movimento do rato (hover)
reageEventos (EventMotion (mx,my)) s =
  let sMain = hoverMain s
      -- mesma lógica de escala que em Desenhar.hs: Tournament -> 1.0, Normal -> 0.7
      sScale = if sMain == 1 then 1.0 else 0.7
      flagBoxW = refW * sScale
      flagBoxH = refH * sScale
      flagCenterY = -20 + 20  -- cy + 20 (mesmo valor usado em Desenhar.hs)
  in case menu s of
       MainMenu
         | dentro mx my 0     -> s { hoverMain = 0, hoverArrow = -1, hoverFlag = False }
         | dentro mx my (-60) -> s { hoverMain = 1, hoverArrow = -1, hoverFlag = False }
         | dentro mx my (-120)-> s { hoverMain = 2, hoverArrow = -1, hoverFlag = False }
         | otherwise          -> s { hoverMain = -1, hoverArrow = -1, hoverFlag = False }

       CountrySelect
         | dentroRect mx my leftArrowX arrowY arrowW arrowH ->
             s { hoverArrow = 0, hoverMain = -1, hoverFlag = False }
         | dentroRect mx my rightArrowX arrowY arrowW arrowH ->
             s { hoverArrow = 1, hoverMain = -1, hoverFlag = False }
         | dentroRect mx my 0 flagCenterY flagBoxW flagBoxH ->
             s { hoverFlag = True, hoverArrow = -1, hoverMain = -1 }
         | otherwise ->
             s { hoverArrow = -1, hoverMain = -1, hoverFlag = False }

       _ -> s { hoverArrow = -1, hoverMain = -1, hoverFlag = False }

reageEventos _ s = s

-- utilitários
dentro :: Float -> Float -> Float -> Bool
dentro mx my y = mx >= -150 && mx <= 150 && my >= (y - 20) && my <= (y + 20)

dentroRect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
dentroRect mx my cx cy w h =
  mx >= (cx - w/2) && mx <= (cx + w/2) && my >= (cy - h/2) && my <= (cy + h/2)
















