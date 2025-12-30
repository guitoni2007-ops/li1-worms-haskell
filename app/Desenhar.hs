module Desenhar
  ( desenha
  ) where

import Graphics.Gloss
import Worms
import Labs2025
import Data.List (elemIndex)

paises :: [String]
paises =
  [ "Portugal", "Brasil", "Argentina", "Franca"
  , "Alemanha", "Espanha", "Inglaterra", "Japao"
  ]

menuBg :: Color
menuBg = greyN 0.80

-- assinatura:
-- 1) fundo do menu
-- 2) fundo do bracket
-- 3) título
-- 4) flags para o menu/seleção
-- 5) flags para a bracket
-- 6...) restantes imagens, por fim Worms
desenha :: Maybe Picture -> Maybe Picture -> Maybe Picture -> [Maybe Picture] -> [Maybe Picture] -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Worms -> Picture
desenha mMenuBg mBracketBg mTitlePic flagsMenu flagsBracket mWormPic mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic w =
  case estadoJogo w of
    Just st -> desenhaEstado mWormPic mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic st
    Nothing ->
      -- quando estamos no ecrã das brackets (Game + showWhite), usa apenas o fundo das brackets e as bandeiras específicas para a bracket
      if menu w == Game && showWhite w
        then
          let bgPic = case mBracketBg of
                        Just p  -> Scale 1 1 p
                        Nothing -> Translate 0 0 $ color menuBg $ rectangleSolid 1920 1080
          in Pictures
               [ bgPic
               , drawQuarterFinals flagsBracket (bracket w) w
               ]
        else
          -- resto do menu principal: usa o fundo normal e as flags do menu/seleção
          let bgPic = case mMenuBg of
                        Just p  -> Scale 1 1 p
                        Nothing -> Translate 0 0 $ color menuBg $ rectangleSolid 1920 1080
          in Pictures
               [ bgPic
               , Translate 0 350 $ Scale 0.9 0.9 $
                   case mTitlePic of
                     Just p  -> p
                     Nothing -> Translate 0 0 $ Scale 0.001 0.001 $ Text "WORMS WORLD CUP"
               , case menu w of
                   MainMenu      -> desenhaMain (hoverMain w)
                   CountrySelect -> desenhaCountrySelect flagsMenu w
                   Game          -> desenhaGameScreen flagsMenu w
               ]

desenhaEstado :: Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Estado -> Picture
desenhaEstado mWormPic mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic st =
  let mapa = mapaEstado st
      (rows, cols) = (length mapa, if null mapa then 0 else length (head mapa))
      winW = 1280
      winH = 720
      margin = 40
      availW = fromIntegral winW - 2 * margin
      availH = fromIntegral winH - 2 * margin
      tileW = if cols == 0 then 0 else availW / fromIntegral cols
      tileH = if rows == 0 then 0 else availH / fromIntegral rows
      tileSize = min tileW tileH
      gridW = tileSize * fromIntegral cols
      gridH = tileSize * fromIntegral rows
      originX = - gridW / 2
      originY = gridH / 2
      tiles = Pictures
        [ Translate (originX + (fromIntegral c + 0.5) * tileSize)
                    (originY - (fromIntegral r + 0.5) * tileSize)
            $ Color (terrenoColor terreno) $ rectangleSolid tileSize tileSize
        | (r, linha) <- zip [0..] mapa
        , (c, terreno) <- zip [0..] linha
        ]
      gridLines = Color (withAlpha 0.08 black) $
        Pictures $
          [ Translate 0 (originY - (fromIntegral i) * tileSize) $ rectangleSolid gridW 1
          | i <- [0..rows] ] ++
          [ Translate (originX + (fromIntegral j) * tileSize) 0 $ rectangleSolid 1 gridH
          | j <- [0..cols] ]
      objetosPics = Pictures $ map (drawObjeto tileSize originX originY mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic) (objetosEstado st)
      minhocasPics = Pictures $ map (drawMinhoca tileSize originX originY mWormPic) (minhocasEstado st)
  in Pictures [tiles, gridLines, objetosPics, minhocasPics]

terrenoColor :: Terreno -> Color
terrenoColor Ar    = makeColor 0.85 0.95 1.0 1.0
terrenoColor Agua  = makeColor 0.2 0.5 0.9 1.0
terrenoColor Terra = makeColor 0.55 0.35 0.15 1.0
terrenoColor Pedra = greyN 0.45
terrenoColor Lava  = makeColor 0.9 0.35 0.1 1.0

drawObjeto :: Float -> Float -> Float -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Objeto -> Picture
drawObjeto tileSize originX originY mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic obj =
  case obj of
    Barril { posicaoBarril = (l,c), explodeBarril = exploding } ->
      let x = originX + (fromIntegral c + 0.5) * tileSize
          y = originY - (fromIntegral l + 0.5) * tileSize
      in case (exploding, mBarrilExplodirPic, mBarrilPic) of
           (True, Just pExpl, _) ->
             let imgScale = (tileSize * 0.9) / 850.0
             in Translate x y $ Scale imgScale imgScale pExpl
           (False, _, Just p) ->
             let imgScale = (tileSize * 0.9) / 850.0
             in Translate x y $ Scale imgScale imgScale p
           _ -> Blank

    Disparo { posicaoDisparo = (l,c), tipoDisparo = t } ->
      let x = originX + (fromIntegral c + 0.5) * tileSize
          y = originY - (fromIntegral l + 0.5) * tileSize
      in case t of
           Mina ->
             case mMinaPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 650.0
                     imgScale  = baseScale * 0.5
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           Jetpack ->
             case mJetpackPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 350.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           Dinamite ->
             case mDinamitePic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 750.0
                     imgScale  = baseScale * 0.60
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           Bazuca ->
             case mBazucaPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 550.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           Escavadora ->
             case mEscavadoraPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 375.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank

drawMinhoca :: Float -> Float -> Float -> Maybe Picture -> Minhoca -> Picture
drawMinhoca tileSize originX originY mWormPic minhoca =
  case posicaoMinhoca minhoca of
    Nothing -> Blank
    Just (r, c) ->
      let x = originX + (fromIntegral c + 0.5) * tileSize
          y = originY - (fromIntegral r + 0.5) * tileSize
          radius = tileSize * 0.35
          alive = case vidaMinhoca minhoca of
                    Viva _ -> True
                    _      -> False
          img = case mWormPic of
                  Just p ->
                    let imgScale = (radius * 2) / 550.0
                    in Translate x y $ Scale imgScale imgScale p
                  Nothing ->
                    Translate x y $ Color (if alive then green else greyN 0.4) $ circleSolid radius
      in img

-- Menu principal: Tournament primeiro, depois Definições, depois Exit
desenhaMain :: Int -> Picture
desenhaMain hover =
  Pictures
    [ desenhaBotao "Tournament" 170 (hover == 0)
    , desenhaBotao "Definições"  90  (hover == 1)
    , desenhaBotao "Exit"      10  (hover == 2)
    ]

desenhaBotao :: String -> Float -> Bool -> Picture
desenhaBotao txt y hovered =
  Pictures
    [ color (if hovered then makeColor 0.90 0.30 0.05 1 else white)
        $ Translate 0 y $ rectangleSolid 360 50
    , color black $ Translate 0 y $ rectangleWire 360 50
    , Translate (-70) (y - 12) $ boldText (if hovered then white else black) 0.22 txt
    ]


-- helper para texto "bold" desenhado com múltiplas cópias ligeiramente deslocadas
boldText :: Color -> Float -> String -> Picture
boldText col s txt =
  let offsets = [ (0,0)
                , (0.9,0)
                , (-0.9,0)
                , (0,0.9)
                , (0,-0.9)
                , (0.6,0.6)
                , (-0.6,0.6)
                , (0.6,-0.6)
                , (-0.6,-0.6)
                ]
      scaled = Scale s s
      copies = [ Translate dx dy $ color col $ Text txt | (dx,dy) <- offsets ]
  in scaled $ Pictures copies

desenhaCountrySelect :: [Maybe Picture] -> Worms -> Picture
desenhaCountrySelect flags w =
  let nFlags = length flags
      idx = if nFlags == 0 then 0 else countryIndex w `mod` nFlags
      flag = if nFlags > 0 then flags !! idx else Nothing
      cy = -20
      leftX = -420
      rightX = 420
      leftH = hoverArrow w == 0
      rightH = hoverArrow w == 1
      sTournament = 1.10
      sNormal = 0.80
      s = if tournament w then sTournament else sNormal
      refW = 276
      refH = 183
      orange = makeColor 0.90 0.30 0.05 1
      outerMargin = 6
      -- escala das setas (aumentada)
      arrowScale = 1.4
  in Pictures
       [ -- destaque laranja atrás da bandeira (como antes)
         if hoverFlag w
           then Translate 0 (cy + 20) $ Scale s s $
                color orange $ rectangleSolid (refW + outerMargin) (refH + outerMargin)
           else Blank
       , case flag of
           Just p  -> Translate 0 (cy + 20) $ Scale s s p
           Nothing -> Translate 0 (cy + 20) $ Scale 0.3 0.3 $ color black $ Text "Bandeira indisponivel"
       , -- contorno laranja (como antes)
         if hoverFlag w
           then Translate 0 (cy + 20) $ Scale s s $ color orange $ rectangleWire (refW + outerMargin) (refH + outerMargin)
           else Blank
       , -- "Play" branco e grosso quando hover
         if hoverFlag w
           then Translate (-40) (-170) $ boldText white 0.40 "Play"
           else Blank
       , -- setas agora maiores e pretas
         Translate leftX cy $ Scale arrowScale arrowScale $ color black $ polygon [(-20,0),(20,30),(20,-30)]
       , Translate rightX cy $ Scale arrowScale arrowScale $ color black $ polygon [(20,0),(-20,30),(-20,-30)]
       , Translate leftX cy $ Scale arrowScale arrowScale $ color black $ line [(-20,0),(20,30),(20,-30),(-20,0)]
       , Translate rightX cy $ Scale arrowScale arrowScale $ color black $ line [(20,0),(-20,30),(-20,-30),(20,0)]
       ]

desenhaGameScreen :: [Maybe Picture] -> Worms -> Picture
desenhaGameScreen _ w =
  let idx = countryIndex w
      nome = if idx >= 0 && idx < length paises then paises !! idx else "Nenhum pais escolhido"
  in Translate 0 0 $ Scale 0.4 0.4 $ color black $ Text ("Jogo: " ++ nome)

-- drawQuarterFinals usa a lista flagsBracket passada desde Main.hs
drawQuarterFinals :: [Maybe Picture] -> Maybe (Bracket String) -> Worms -> Picture
drawQuarterFinals _ Nothing _ = Blank
drawQuarterFinals flags (Just b) w =
  let firstRound = if null (rounds b) then [] else head (rounds b)
      rowSpacing = 270
      startX = -295
      startY = 140
      picScale = 0.38
      refW = 276
      refH = 183
      flagW = refW * picScale
      flagH = refH * picScale

      flagForName :: String -> Maybe Picture
      flagForName name =
        case elemIndex name paises of
          Just i -> if i < length flags then flags !! i else Nothing
          Nothing -> Nothing

      drawFlag :: Maybe Picture -> Picture
      drawFlag (Just p) = Scale picScale picScale p
      drawFlag Nothing  = Pictures
        [ color white $ rectangleSolid flagW flagH
        , color black $ rectangleWire flagW flagH
        ]

      drawMatch :: Match String -> Float -> Float -> Picture
      drawMatch (Match a b) x y =
        let sep = flagH / 2 + 30
            topY = y + sep
            botY = y - sep
            top = Translate x topY $ drawFlag (flagForName a)
            bot = Translate x botY $ drawFlag (flagForName b)
        in Pictures [top, bot]

      half = length firstRound `div` 2
      leftMatches = take half firstRound
      rightMatches = drop half firstRound
      leftX = startX - 480
      rightX = startX + 1070

      drawSide ms x = Pictures $ zipWith (\m i ->
        let y = startY - fromIntegral i * rowSpacing
        in drawMatch m x y) ms [0..]

      playW = 220
      playH = 64
      playY = -260
      isHoverPlay = hoverPlay w
      playFill = if isHoverPlay then makeColor 0.90 0.30 0.05 1 else white
      playTextPic = if isHoverPlay then boldText white 0.35 "Play" else Scale 0.35 0.35 (color black $ Text "Play")
      playButton = Translate 0 playY $
        Pictures
          [ color playFill $ rectangleSolid playW playH
          , color black $ rectangleWire playW playH
          , Translate (-40) (-12) playTextPic
          ]

  in Pictures [ drawSide leftMatches leftX, drawSide rightMatches rightX, playButton ]

