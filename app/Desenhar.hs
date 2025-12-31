module Desenhar
  ( desenha
  ) where

import Graphics.Gloss
import qualified Labs2025 as L25
import qualified Worms as W
import Worms (Worms)
import Data.List (elemIndex)

paises :: [String]
paises =
  [ "Portugal", "Brasil", "Argentina", "Franca"
  , "Alemanha", "Espanha", "Inglaterra", "Japao"
  ]

menuBg :: Color
menuBg = greyN 0.80

-- assinatura: 14 imagens/valores + Worms -> Picture
desenha :: Maybe Picture      -- mMenuBg
       -> Maybe Picture      -- mBracketBg
       -> Maybe Picture      -- mGameBg
       -> Maybe Picture      -- mTitlePic
       -> [Maybe Picture]    -- flagsMenu
       -> [Maybe Picture]    -- flagsBracket
       -> [Maybe Picture]    -- mWormPics
       -> Maybe Picture      -- mBarrilPic
       -> Maybe Picture      -- mBarrilExplodirPic
       -> Maybe Picture      -- mMinaPic
       -> Maybe Picture      -- mJetpackPic
       -> Maybe Picture      -- mDinamitePic
       -> Maybe Picture      -- mBazucaPic
       -> Maybe Picture      -- mEscavadoraPic
       -> Worms
       -> Picture
desenha mMenuBg mBracketBg mGameBg mTitlePic flagsMenu flagsBracket mWormPics mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic w =
  case W.estadoJogo w of
    -- jogo a decorrer: desenha o fundo do jogo (mGameBg) atrás do mapa/objetos/minhocas
    Just st ->
      let bgPic = case mGameBg of
                    Just p  -> Scale 1 1 p
                    Nothing -> Translate 0 0 $ color menuBg $ rectangleSolid 1920 1080
          gamePic = desenhaEstado mWormPics mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic st
          timerPic = drawTurnTimer w
          sidePanel = drawSidePanel mWormPics mBazucaPic mMinaPic mJetpackPic mEscavadoraPic mDinamitePic w
      in Pictures [ bgPic, gamePic, timerPic, sidePanel ]

    -- não há estado de jogo: estamos no menu/bracket
    Nothing ->
      -- se estamos em modo torneio e devemos mostrar a bracket (tournament + showWhite usado como trigger)
      if W.menu w == W.Game && W.showWhite w && W.tournament w && W.lastWinner w == Nothing
        then
          -- ecrã da bracket: usa mBracketBg como fundo e desenha os quartos com flags
          let bgPic = case mBracketBg of
                        Just p  -> Scale 1 1 p
                        Nothing -> Translate 0 0 $ color menuBg $ rectangleSolid 1920 1080
          in Pictures
               [ bgPic
               , drawQuarterFinals flagsBracket (W.bracket w) w
               , drawBracketButtons w
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
               , case W.menu w of
                   W.MainMenu      -> desenhaMain (W.hoverMain w)
                   W.CountrySelect -> desenhaCountrySelect flagsMenu w
                   W.Game          -> desenhaGameScreen flagsMenu w
               ]

-- Botões da bracket: Replay (centro) e Back (embaixo)
drawBracketButtons :: Worms -> Picture
drawBracketButtons w =
  let replayY = -120
      replayW = 220
      replayH = 64
      replayFill = if W.hoverPlay w then makeColor 0.90 0.30 0.05 1 else white
      replayText = if W.hoverPlay w then boldText white 0.28 "Replay" else Scale 0.28 0.28 (color black $ Text "Replay")
      replayPic = Translate 0 replayY $
        Pictures [ color replayFill $ rectangleSolid replayW replayH
                 , color black $ rectangleWire replayW replayH
                 , Translate (-40) (-12) replayText
                 ]

      backY = -260
      backW = 160
      backH = 48
      backFill = white
      backText = Scale 0.22 0.22 (color black $ Text "Back")
      backPic = Translate 0 backY $
        Pictures [ color backFill $ rectangleSolid backW backH
                 , color black $ rectangleWire backW backH
                 , Translate (-28) (backY - 10) backText
                 ]
  in Pictures [ replayPic, backPic ]

-- Função desenhaEstado (mantive a tua versão anterior, centrada)
desenhaEstado :: [Maybe Picture] -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> L25.Estado -> Picture
desenhaEstado mWormPics mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic st =
  let mapa = L25.mapaEstado st
      (rows, cols) = (length mapa, if null mapa then 0 else length (head mapa))
      -- AUMENTEI AQUI: área de jogo maior (como tinhas antes)
      winW = 1800
      winH = 1010
      -- margem reduzida para aumentar o tamanho do grid (opção recomendada)
      margin = 10
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
      objetosPics = Pictures $ map (drawObjeto tileSize originX originY mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic) (L25.objetosEstado st)
      minhocasPics = Pictures $ zipWith (\m i -> drawMinhoca tileSize originX originY mWormPics m i) (L25.minhocasEstado st) [0..]
  in Pictures [tiles, gridLines, objetosPics, minhocasPics]

terrenoColor :: L25.Terreno -> Color
terrenoColor t =
  case t of
    L25.Ar    -> makeColor 0.85 0.95 1.0 1.0
    L25.Agua  -> makeColor 0.2 0.5 0.9 1.0
    L25.Terra -> makeColor 0.55 0.35 0.15 1.0
    L25.Pedra -> greyN 0.45
    L25.Lava  -> makeColor 0.9 0.35 0.1 1.0

drawObjeto :: Float -> Float -> Float -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> L25.Objeto -> Picture
drawObjeto tileSize originX originY mBarrilPic mBarrilExplodirPic mMinaPic mJetpackPic mDinamitePic mBazucaPic mEscavadoraPic obj =
  case obj of
    L25.Barril { L25.posicaoBarril = (l,c), L25.explodeBarril = exploding } ->
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

    L25.Disparo { L25.posicaoDisparo = (l,c), L25.tipoDisparo = t } ->
      let x = originX + (fromIntegral c + 0.5) * tileSize
          y = originY - (fromIntegral l + 0.5) * tileSize
      in case t of
           L25.Mina ->
             case mMinaPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 650.0
                     imgScale  = baseScale * 0.5
                 in Translate x (y+22) $ Scale imgScale imgScale p
               Nothing -> Blank
           L25.Jetpack ->
             case mJetpackPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 350.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           L25.Dinamite ->
             case mDinamitePic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 750.0
                     imgScale  = baseScale * 0.60
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           L25.Bazuca ->
             case mBazucaPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 550.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank
           L25.Escavadora ->
             case mEscavadoraPic of
               Just p ->
                 let baseScale = (tileSize * 0.85) / 375.0
                     imgScale  = baseScale * 0.65
                 in Translate x y $ Scale imgScale imgScale p
               Nothing -> Blank

drawMinhoca :: Float -> Float -> Float -> [Maybe Picture] -> L25.Minhoca -> Int -> Picture
drawMinhoca tileSize originX originY mWormPics minhoca idx =
  case L25.posicaoMinhoca minhoca of
    Nothing -> Blank
    Just (r, c) ->
      let x = originX + (fromIntegral c + 0.5) * tileSize
          y = originY - (fromIntegral r + 0.5) * tileSize
          radius = tileSize * 0.35
          alive = case L25.vidaMinhoca minhoca of
                    L25.Viva _ -> True
                    _          -> False
          mPic = if idx >= 0 && idx < length mWormPics then mWormPics !! idx else Nothing
      in case mPic of
           Just p ->
             let imgScale = (radius * 2) / 550.0
             in Translate x y $ Scale imgScale imgScale p
           Nothing ->
             Translate x y $ Color (if alive then green else greyN 0.4) $ circleSolid radius

-- Menu principal: Quick Play, Tournament, Exit (3 botões)
desenhaMain :: Int -> Picture
desenhaMain hover =
  Pictures
    [ desenhaBotao "Quick Play" 170 (hover == 0)
    , desenhaBotao "Tournament" 90  (hover == 1)
    , desenhaBotao "Exit"      10  (hover == 2)
    ]

-- desenhaBotao: botão com destaque laranja ao passar o rato
desenhaBotao :: String -> Float -> Bool -> Picture
desenhaBotao txt y hovered =
  let hoverFill = makeColor 0.95 0.45 0.10 1   -- laranja mais vivo para hover
      normalFill = white
      borderCol = black
      textCol = if hovered then white else black
      boxW = 360
      boxH = 50
  in Pictures
       [ color (if hovered then hoverFill else normalFill) $ Translate 0 y $ rectangleSolid boxW boxH
       , color borderCol $ Translate 0 y $ rectangleWire boxW boxH
       , Translate (-70) (y - 12) $ boldText textCol 0.22 txt
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
      idx = if nFlags == 0 then 0 else W.countryIndex w `mod` nFlags
      flag = if nFlags > 0 then flags !! idx else Nothing
      cy = -20
      leftX = -420
      rightX = 420
      sTournament = 1.10
      sNormal = 0.80
      s = if W.tournament w then sTournament else sNormal
      refW = 276
      refH = 183
      orange = makeColor 0.90 0.30 0.05 1
      outerMargin = 6
      arrowScale = 1.4
  in Pictures
       [ if W.hoverFlag w
           then Translate 0 (cy + 20) $ Scale s s $
                color orange $ rectangleSolid (refW + outerMargin) (refH + outerMargin)
           else Blank
       , case flag of
           Just p  -> Translate 0 (cy + 20) $ Scale s s p
           Nothing -> Translate 0 (cy + 20) $ Scale 0.3 0.3 $ color black $ Text "Bandeira indisponivel"
       , if W.hoverFlag w
           then Translate 0 (cy + 20) $ Scale s s $ color orange $ rectangleWire (refW + outerMargin) (refH + outerMargin)
           else Blank
       , if W.hoverFlag w
           then Translate (-40) (-170) $ boldText white 0.40 "Play"
           else Blank
       , Translate leftX cy $ Scale arrowScale arrowScale $ color black $ polygon [(-20,0),(20,30),(20,-30)]
       , Translate rightX cy $ Scale arrowScale arrowScale $ color black $ polygon [(20,0),(-20,30),(-20,-30)]
       , Translate leftX cy $ Scale arrowScale arrowScale $ color black $ line [(-20,0),(20,30),(20,-30),(-20,0)]
       , Translate rightX cy $ Scale arrowScale arrowScale $ color black $ line [(20,0),(-20,30),(-20,-30),(20,0)]
       ]

desenhaGameScreen :: [Maybe Picture] -> Worms -> Picture
desenhaGameScreen _ w =
  let idx = W.countryIndex w
      nome = if idx >= 0 && idx < length paises then paises !! idx else "Nenhum pais escolhido"
  in Translate 0 0 $ Scale 0.4 0.4 $ color black $ Text ("Jogo: " ++ nome)

-- drawQuarterFinals usa a lista flagsBracket passada desde Main.hs
drawQuarterFinals :: [Maybe Picture] -> Maybe (W.Bracket String) -> Worms -> Picture
drawQuarterFinals _ Nothing _ = Blank
drawQuarterFinals flags (Just b) w =
  let firstRound = if null (W.rounds b) then [] else head (W.rounds b)
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

      drawMatch :: W.Match String -> Float -> Float -> Picture
      drawMatch (W.Match a b) x y =
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
      isHoverPlay = W.hoverPlay w
      playFill = if isHoverPlay then makeColor 0.90 0.30 0.05 1 else white
      playTextPic = if isHoverPlay then boldText white 0.35 "Play" else Scale 0.35 0.35 (color black $ Text "Play")
      playButton = Translate 0 playY $
        Pictures
          [ color playFill $ rectangleSolid playW playH
          , color black $ rectangleWire playW playH
          , Translate (-40) (-12) playTextPic
          ]

  in Pictures [ drawSide leftMatches leftX, drawSide rightMatches rightX, playButton ]

-- Desenha apenas a barra de progresso do turno (sem caixa preta), com texto do jogador
drawTurnTimer :: Worms -> Picture
drawTurnTimer w =
  let -- posição e dimensões (ajusta conforme necessário)
      x = 760    -- posição horizontal da barra (mais à esquerda)
      y = 200    -- posição vertical da barra
      boxW = 220 -- largura de referência (não desenhamos o box)
      pad = 8
      innerW = boxW - 2 * pad
      innerH = 24

      -- valores do turno
      ticksLeft = W.turnTicksLeft w
      ticksTotal = W.turnDuration w
      pct = if ticksTotal <= 0 then 0 else max 0 (min 1 (fromIntegral ticksLeft / fromIntegral ticksTotal))

      -- cores
      fg = makeColor 0.90 0.30 0.05 1
      barBg = greyN 0.2
      textCol = white

      -- posição da barra (centro em x,y)
      barX = x
      barY = y

      -- fundo da barra
      barBgPic = Translate barX barY $ color barBg $ rectangleSolid innerW innerH

      -- preenchimento laranja (alinhado à esquerda do fundo)
      barFillW = innerW * pct

      -- deslocamento extra para a direita do preenchimento (ajusta se necessário)
      fillOffset :: Float
      fillOffset = 8.0

      barFillPic = Translate (barX - innerW/2 + barFillW/2 + fillOffset) barY $ color fg $ rectangleSolid barFillW innerH

      -- texto do jogador (1-based), maior e em negrito, ligeiramente acima
      playerIdx = Translate (barX - innerW/2) (barY + 40) $
                  boldText textCol 0.28 ("Jogador " ++ show (W.currentTurn w + 1))

  in Pictures [barBgPic, barFillPic, playerIdx]

-- Novo: desenha painel lateral com duas minhocas e os seus inventários (ícone + número)
drawSidePanel :: [Maybe Picture] -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Maybe Picture -> Worms -> Picture
drawSidePanel mWormPics mBazucaPic mMinaPic _mJetpackPic _mEscavadoraPic mDinamitePic w =
  let x = -800
      yTop = 220
      yGap = 400
      portraitSize = 140
      iconSize = 28
      textScale = 0.14
      mEst = W.estadoJogo w
      minhList = case mEst of
                   Just est -> L25.minhocasEstado est
                   Nothing  -> []
      getMinh i field =
        if i >= 0 && i < length minhList
          then field (minhList !! i)
          else 0
      drawIconLocal :: Maybe Picture -> Float -> Picture
      drawIconLocal mPic size =
        case mPic of
          Just p  -> Scale (size/550) (size/550) p
          Nothing -> Color (greyN 0.8) $ rectangleSolid size size
      drawEntry i posY =
        let portraitPic = if i >= 0 && i < length mWormPics
                            then case mWormPics !! i of
                                   Just p -> Scale (portraitSize/550) (portraitSize/550) p
                                   Nothing -> Translate 0 0 $ color white $ circleSolid (portraitSize/2)
                            else Translate 0 0 $ color white $ circleSolid (portraitSize/2)
            baz = getMinh i L25.bazucaMinhoca
            minas = getMinh i L25.minaMinhoca
            din = getMinh i L25.dinamiteMinhoca
            portrait = Translate x posY portraitPic
            invX = x + portraitSize/2 + 80
            invYStart = posY + portraitSize/2
            itemGap = 70
            bgLeft = x - portraitSize / 2 - 30
            bgRight = invX + 60
            bgW = bgRight - bgLeft
            bgCX = (bgLeft + bgRight) / 2
            bgCY = posY + portraitSize / 2 - 70
            bgH = portraitSize + 130
            bgRect = Translate bgCX bgCY $ color (greyN 0.50) $ rectangleSolid bgW bgH
            invLineIcon :: Float -> Maybe Picture -> Int -> Picture
            invLineIcon dy mPic count =
              Translate invX (invYStart - dy) $
                Pictures
                  [ Translate (-20) 0 $ drawIconLocal mPic iconSize
                  , Translate 10 0 $ boldText white 0.18 (show count)
                  ]
            invPics = Pictures
              [ invLineIcon (0 * itemGap)   mBazucaPic baz
              , invLineIcon (1 * itemGap)   mMinaPic minas
              , invLineIcon (2 * itemGap)   mDinamitePic din
              ]
        in Pictures [ bgRect, portrait, invPics ]
  in Pictures [ drawEntry 0 yTop, drawEntry 1 (yTop - yGap) ]















