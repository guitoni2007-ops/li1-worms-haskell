module Desenhar
  ( desenha
  ) where

import Graphics.Gloss
import Worms

paises :: [String]
paises =
  [ "Portugal", "Brasil", "Argentina", "Franca"
  , "Alemanha", "Espanha", "Inglaterra", "Japao"
  ]

-- desenha título + ecrã conforme menu
desenha :: Picture -> [Maybe Picture] -> Worms -> Picture
desenha titlePic flags w =
  Pictures
    [ Translate 0 160 $ Scale 0.9 0.9 titlePic
    , case menu w of
        MainMenu      -> desenhaMain (hoverMain w)
        CountrySelect -> desenhaCountrySelect flags w
        Game          -> desenhaGameScreen flags w
    ]

-- menu principal (3 botões)
desenhaMain :: Int -> Picture
desenhaMain hover =
  Pictures
    [ desenhaBotao "Classic"     0     (hover == 0)
    , desenhaBotao "Tournament" (-60)  (hover == 1)
    , desenhaBotao "Exit"       (-120) (hover == 2)
    ]

desenhaBotao :: String -> Float -> Bool -> Picture
desenhaBotao txt y hovered =
  Pictures
    [ color (if hovered then makeColor 0.90 0.30 0.05 1 else greyN 0.8)
        $ Translate 0 y $ rectangleSolid 300 40
    , color black $ Translate 0 y $ rectangleWire 300 40
    , Translate (-120) (y - 10) $ Scale 0.2 0.2 $ color (if hovered then white else black) $ Text txt
    ]

-- ecrã de seleção de país (bandeira central; no Tournament mostramos só a bandeira maior)
desenhaCountrySelect :: [Maybe Picture] -> Worms -> Picture
desenhaCountrySelect flags w =
  let idx = countryIndex w `mod` length paises
      flag = if idx < length flags then flags !! idx else Nothing
      cy = -20
      leftX = -420
      rightX = 420
      leftH = hoverArrow w == 0
      rightH = hoverArrow w == 1
      -- escala maior para o modo Tournament (apenas bandeira)
      sTournament = 1.0
      sNormal = 0.7
      s = if hoverMain w == 1 then sTournament else sNormal
      -- referência de tamanho da "caixa" da bandeira (ajusta se quiseres)
      refW = 520
      refH = 300
      margin = 6
      boxW = refW * s + margin
      boxH = refH * s + margin
      orange = makeColor 0.90 0.30 0.05 1
  in Pictures
       [ Translate 0 cy $ color (greyN 0.95) $ rectangleSolid 980 380
       , Translate 0 cy $ color black $ rectangleWire 980 380
       , case flag of
           Just p  -> Translate 0 (cy + 20) $ Scale s s p
           Nothing -> Translate 0 (cy + 20) $ Scale 0.3 0.3 $ color black $ Text "Bandeira indisponivel"
       -- contorno laranja quando o rato está sobre a bandeira
       , if hoverFlag w
           then Translate 0 (cy + 20) $ color orange $ rectangleWire boxW boxH
           else Blank
       -- setas e contornos
       , Translate leftX cy $ color (if leftH then orange else greyN 0.8) $ polygon [(-20,0),(20,30),(20,-30)]
       , Translate rightX cy $ color (if rightH then orange else greyN 0.8) $ polygon [(20,0),(-20,30),(-20,-30)]
       , Translate leftX cy $ color black $ line [(-20,0),(20,30),(20,-30),(-20,0)]
       , Translate rightX cy $ color black $ line [(20,0),(-20,30),(-20,-30),(20,0)]
       ]

-- ecrã de jogo simples (placeholder)
desenhaGameScreen :: [Maybe Picture] -> Worms -> Picture
desenhaGameScreen _ w =
  let idx = countryIndex w
      nome = if idx >= 0 && idx < length paises then paises !! idx else "Nenhum pais escolhido"
  in Translate 0 0 $ Scale 0.4 0.4 $ color black $ Text ("Jogo: " ++ nome)




    














