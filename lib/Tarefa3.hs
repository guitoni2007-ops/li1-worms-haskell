{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either

import Labs2025

import Tarefa0_2025

import Tarefa1
import Tarefa2

type Dano = Int
type Danos = [(Posicao,Dano)]


estadoTeste = Estado mapaTesteValido1 [] minhocasTeste14



mapa7x7Ar =
  [ [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
  ]


mapaTesteSoAr =
  [ [Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar]
  ]

mapaTesteValido1 =
  [ [Ar, Ar, Terra, Pedra, Ar]
  , [Agua, Terra, Terra, Pedra, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua ,Ar]
  , [Ar, Ar, Terra, Pedra, Ar]
  ]


mapaTesteValido2 =
  [ [Terra, Terra, Terra]
  , [Ar,    Ar,    Ar]
  , [Terra,  Ar,    Terra]
  ]



objetosTeste14 =
  ( Disparo (4,4) Sul Mina (Just 0) 99)

objetosTeste7 =
  ( Barril (3,0) False )

objetosTeste9 =
  [ Disparo (1,0) Norte Dinamite (Just 1) 0
  , Disparo (0,0) Sul Mina Nothing 0
  ]


minhocasTeste12 =
  Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1

minhocasTeste14 =
  [ Minhoca (Just (2,3)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (3,2)) (Viva 80) 1 1 1 1 1
  , Minhoca (Just (3,3)) (Viva 80) 1 1 1 1 1
  ]



















-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'

-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca (Estado mapa _ _) _ minhoca = minhocaComGravidade mapa minhoca

-- | Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda, uma lista de posições afetadas com o dano associado.
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto e i o = undefined

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos ds e = undefined



--Verifica se uma minhoca está viva.
minhocaViva :: Minhoca -> Bool
minhocaViva minhoca =
  case vidaMinhoca minhoca of
    Viva _ -> True
    Morta  -> False

--Calcula a nova posição da minhoca passado um tick que é afetada pela gravidade. 
posicaoAbaixo :: Posicao -> Posicao
posicaoAbaixo (l, c) = (l + 1, c)

--Obtém a posição abaixo da minhoca (se tiver posição).
posicaoAbaixoMinhoca :: Minhoca -> Maybe Posicao
posicaoAbaixoMinhoca minhoca =
  case posicaoMinhoca minhoca of
    Just pos -> Just (posicaoAbaixo pos)
    Nothing  -> Nothing

--Obtém o terreno abaixo da minhoca.
terrenoAbaixoMinhoca :: Mapa -> Minhoca -> Maybe Terreno
terrenoAbaixoMinhoca mapa minhoca =
  case posicaoAbaixoMinhoca minhoca of
    Just pos ->
      if ePosicaoMatrizValida pos mapa then Just (mapa !! fst pos !! snd pos)
      else Nothing
    Nothing -> Nothing

--Aplica os efeitos da gravidade numa minhoca.
aplicaGravidadeMinhoca :: Mapa -> Minhoca -> Minhoca
aplicaGravidadeMinhoca mapa minhoca =
  case posicaoMinhoca minhoca of
    Just pos ->
      let novaPos = posicaoAbaixo pos
      in case terrenoAbaixoMinhoca mapa minhoca of
           Nothing     -> minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
           Just Agua   -> minhoca { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
           Just Ar     -> minhoca { posicaoMinhoca = Just novaPos }
           Just _      -> minhoca
    Nothing -> minhoca

--Verifica se uma minhoca tem um terreno válido para ser afetada pela gravidade.
eMinhocaGravidade :: Mapa -> Minhoca -> Bool
eMinhocaGravidade mapa minhoca =
  case posicaoMinhoca minhoca of
    Just pos ->
      ePosicaoMatrizValida pos mapa &&
      ePosicaoMapaLivre pos mapa
    Nothing -> False

--Serve para evitar aplicar gravidade a minhocas mortas ou que estão sobre terreno opaco.
deveAplicarGravidade :: Mapa -> Minhoca -> Bool
deveAplicarGravidade mapa minhoca =
  minhocaViva minhoca && eMinhocaGravidade mapa minhoca

--Aplica gravidade à minhoca se estiver viva e sobre terreno não opaco.
minhocaComGravidade :: Mapa -> Minhoca -> Minhoca
minhocaComGravidade mapa minhoca
  | deveAplicarGravidade mapa minhoca = aplicaGravidadeMinhoca mapa minhoca
  | otherwise = minhoca

--Verifica se um objeto deve explodir imediatamente.
deveExplodirObjeto :: Mapa -> Objeto -> Bool
deveExplodirObjeto mapa obj =
  case obj of
    Barril _ True -> True
    Disparo pos dir Bazuca (Just 0) _ -> not (posicaoValidaOuBazuca  mapa pos dir)
    _ -> False

--Filtra os objetos que devem explodir imediatamente.
filtraObjetosExplosivos :: Mapa -> [Objeto] -> [Objeto]
filtraObjetosExplosivos mapa objs = filter (deveExplodirObjeto mapa) objs

--Retorna o raio de explosão de um objeto.
raioExplosao :: Objeto -> Int
raioExplosao obj =
  case obj of
    Barril{}   -> 5
    Disparo _ _ Bazuca _ _ -> 5
    Disparo _ _ Dinamite _ _ -> 7
    Disparo _ _ Mina _ _ -> 3
    _ -> 0

-- | Devolve as posições afetadas por uma explosão de diâmetro d, ignorando posições fora do mapa.
posicoesAfetadasPorExplosaoValida :: Posicao -> Int -> Mapa -> [Posicao]
posicoesAfetadasPorExplosaoValida (cx, cy) d mapa =
  [ (x, y)
  | x <- [cx - r .. cx + r]
  , y <- [cy - r .. cy + r]
  , let dist = abs (x - cx) + abs (y - cy)
  , dist <= (d - 1) `div` 2
  , ePosicaoMatrizValida (x, y) mapa
  ]
  where
    r = d `div` 2

--Devolve o terreno na posição, se for válida.
terrenoNaPosicao :: Mapa -> Posicao -> Maybe Terreno
terrenoNaPosicao mapa (l, c)
  | ePosicaoMatrizValida (l, c) mapa = Just (mapa !! l !! c)
  | otherwise                        = Nothing

--Verifica se a posição está em terreno Ar ou Água (não opaco).
terrenoNaoOpacoBarril :: Mapa -> Posicao -> Bool
terrenoNaoOpacoBarril mapa pos =
  case terrenoNaPosicao mapa pos of
    Just Ar   -> True
    Just Agua -> True
    _         -> False

--Atualiza o estado de um barril: se estiver em Ar ou Água, passa para prestes a explodir.
atualizaBarril :: Mapa -> Objeto -> Objeto
atualizaBarril mapa (Barril pos False)
  | terrenoNaoOpacoBarril mapa pos = Barril pos True
  | otherwise                      = Barril pos False
atualizaBarril _ barril = barril


--Move a bazuca na direção indicada. Se sair do mapa, é removida.
atualizaDisparoBazuca :: Mapa -> Objeto ->  Maybe Objeto
atualizaDisparoBazuca mapa (Disparo pos dir Bazuca t d) =
  let novaPos = movePosicao dir pos
  in if ePosicaoMatrizValida novaPos mapa
     then Just (Disparo novaPos dir Bazuca t d) -- Muda para a nova posição.
     else Nothing  -- Removido sem explosão.
atualizaDisparoBazuca _ obj = Just obj -- Para outros objetos, ficam na mesma posição

--Verifica se o terreno é do tipo Ar
eTerrenoAr :: Terreno -> Bool
eTerrenoAr Ar = True
eTerrenoAr _     = False

--Verifica se a posição de uma dinamite é afetada pela gravidade 
ePosicaoDinamiteLivre :: Posicao -> Mapa -> Bool
ePosicaoDinamiteLivre (x, y) mapa =
    let terreno = mapa !! x !! y  
    in eTerrenoAr terreno

--Calcula 
rodaPosicaoDirecao1 :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao1 ((l,c),d) | d==Norte = ((l+1,c), Norte)
                             | d==Nordeste = ((l-1,c+1), Este)
                             | d==Este = ((l+1,c+1), Sudeste)
                             | d==Sudeste = ((l+1,c+1), Sul)
                             | d==Sul = ((l+1,c), Sul)
                             | d==Sudoeste = ((l+1,c-1), Sul)
                             | d==Oeste = ((l+1,c-1), Sudoeste)
                             | d==Noroeste = ((l-1,c-1), Oeste)


--Atualiza a dinamite aplicando a rotação de direção e movimento conforme rodaPosicaoDirecao1.
validaEDirecionaDinamiteRoda :: Mapa -> Objeto -> Objeto
validaEDirecionaDinamiteRoda mapa obj@(Disparo pos dir Dinamite t d)
  | ePosicaoMatrizValida novaPos mapa = Disparo novaPos novaDir Dinamite t d
  | otherwise                         = obj
  where (novaPos, novaDir) = rodaPosicaoDirecao1 (pos, dir)
validaEDirecionaDinamiteRoda _ obj = obj

--Atualiza a dinamite se estiver no ar, aplicando rotação e movimento conforme rodaPosicaoDirecao1.
atualizaDinamiteRodaSeNoAr :: Mapa -> Objeto -> Objeto
atualizaDinamiteRodaSeNoAr mapa obj@(Disparo pos dir Dinamite t d)
  | ePosicaoDinamiteLivre pos mapa && ePosicaoMatrizValida novaPos mapa =
      Disparo novaPos novaDir Dinamite t d
  | otherwise = obj
  where
    (novaPos, novaDir) = rodaPosicaoDirecao1 (pos, dir)
atualizaDinamiteRodaSeNoAr _ obj = obj

-- | Atualiza a posição da mina: se estiver em Ar ou Água, cai e aponta para Norte.
atualizaDisparoMina :: Mapa -> Objeto -> Objeto
atualizaDisparoMina mapa obj@(Disparo (l, c) dir Mina t d)
  | ePosicaoMatrizValida (l, c) mapa && not (eTerrenoOpaco (mapa !! l !! c)) =
      Disparo (l + 1, c) Norte Mina t d
  | otherwise = obj
atualizaDisparoMina _ obj = obj

--Verifica se a posição abaixo do disparo está ocupada.
disparoEstaNoChao :: Estado -> Objeto -> Bool
disparoEstaNoChao estado (Disparo (l, c) _ _ _ _) =
  let posAbaixo = (l + 1, c)
  in not (ePosicaoEstadoLivre posAbaixo estado)
disparoEstaNoChao _ _ = False


--Se for mina ou dinamite no chão, fica parado e aponta para Norte.
fixaDisparoNoChao :: Estado -> Objeto -> Objeto
fixaDisparoNoChao estado obj@(Disparo pos _ arma tempo d)
  | arma == Mina || arma == Dinamite
  , disparoEstaNoChao estado obj
  = Disparo pos Norte arma tempo d
fixaDisparoNoChao _ obj = obj

--Atualiza o tempo do objeto se for maior que 0.
atualizaTempoObjeto :: Objeto -> Objeto
atualizaTempoObjeto (Disparo pos dir arma (Just n) d)
  | n > 0     = Disparo pos dir arma (Just (n - 1)) d
  | otherwise = Disparo pos dir arma (Just 0) d
atualizaTempoObjeto obj = obj

-- | Verifica se o objeto é uma mina sem tempo.
eMinaSemTempo :: Objeto -> Bool
eMinaSemTempo (Disparo _ _ Mina Nothing _) = True
eMinaSemTempo _ = False

-- | Verifica se a minhoca é inimiga e está viva.
minhocaInimigaViva :: Int -> Minhoca -> Bool
minhocaInimigaViva dono (Minhoca _ vida num _ _ _ _) =
  num /= dono && minhocaViva (Minhoca Nothing vida num 0 0 0 0)

-- | Verifica se a minhoca está na área de explosão da mina.
minhocaNaAreaExplosao :: Posicao -> Mapa -> Minhoca -> Bool
minhocaNaAreaExplosao centro mapa (Minhoca (Just pos) _ _ _ _ _ _) =
  pos `elem` posicoesAfetadasPorExplosaoValida centro 3 mapa
minhocaNaAreaExplosao _ _ _ = False

-- | Verifica se alguma minhoca inimiga viva está na área de explosão da mina.
existeInimigoNaArea :: Posicao -> Int -> Mapa -> [Minhoca] -> Bool
existeInimigoNaArea centro dono mapa minhocas =
  any (\m -> minhocaInimigaViva dono m && minhocaNaAreaExplosao centro mapa m) minhocas

-- | Ativa mina sem tempo se houver inimigo na área.
ativaMinaSeInimigo :: Mapa -> [Minhoca] -> Objeto -> Objeto
ativaMinaSeInimigo mapa minhocas obj@(Disparo pos _ Mina Nothing dono)
  | existeInimigoNaArea pos dono mapa minhocas = Disparo pos Norte Mina (Just 2) dono
  | otherwise = obj
ativaMinaSeInimigo _ _ obj = obj

-- | Aplica ativação de minas sem tempo a todos os objetos.
ativaMinasPorMinhocasInimigas :: Estado -> [Objeto] -> [Objeto]
ativaMinasPorMinhocasInimigas (Estado mapa _ minhocas) =
  map (ativaMinaSeInimigo mapa minhocas)

distanciaManhattan :: Posicao -> Posicao -> Int
distanciaManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

danoPorDistancia :: Int -> Int -> Int
danoPorDistancia d dist
  | dist == 0  = d * 10
  | otherwise  = max 0 ((d - dist - 1) * 10)

posicoesComDistancia :: Posicao -> Int -> Mapa -> [(Posicao, Int)]
posicoesComDistancia centro d mapa =
  [ (pos, distanciaManhattan centro pos)
  | pos <- posicoesAfetadasPorExplosaoValida centro d mapa
  ]

danoPorExplosao :: Posicao -> Int -> Mapa -> [(Posicao, Int)]
danoPorExplosao centro d mapa =
  [ (pos, dano)
  | (pos, dist) <- posicoesComDistancia centro d mapa
  , let dano = danoPorDistancia d dist
  , dano > 0
  ]






































































{-
--Verifica se um objeto deve explodir imediatamente.
deveExplodirObjeto :: Mapa -> Objeto -> Bool
deveExplodirObjeto mapa obj =
  case obj of
    Barril _ True -> True
    Disparo _ _ _ (Just 0) _ -> True
    Disparo pos _ Bazuca _ _ -> not (ePosicaoMapaLivre pos mapa)
    _ -> False

--Retorna o raio de explosão de um objeto.
raioExplosao :: Objeto -> Int
raioExplosao obj =
  case obj of
    Barril{}   -> 5
    Disparo _ _ Bazuca _ _ -> 5
    Disparo _ _ Dinamite _ _ -> 7
    Disparo _ _ Mina _ _ -> 3
    _ -> 0

--Gera as posições afetadas por uma explosão circular.
posicoesExplosao :: Posicao -> Int -> [Posicao]
posicoesExplosao (x, y) r =
  [ (i, j)
  | i <- [x - r .. x + r]
  , j <- [y - r .. y + r]
  , (i - x)^2 + (j - y)^2 <= r^2
  ]

--Extrai a posição de um objeto.
posicaoObjeto :: Objeto -> Posicao
posicaoObjeto (Barril p _)         = p
posicaoObjeto (Disparo p _ _ _ _)  = p

--Devolve o terreno na posição, se for válida.
terrenoNaPosicao :: Mapa -> Posicao -> Maybe Terreno
terrenoNaPosicao mapa (l, c)
  | ePosicaoMatrizValida (l, c) mapa = Just (mapa !! l !! c)
  | otherwise                        = Nothing

--Verifica se a posição está em terreno Ar ou Água (não opaco).
terrenoNaoOpacoBarril :: Mapa -> Posicao -> Bool
terrenoNaoOpacoBarril mapa pos =
  case terrenoNaPosicao mapa pos of
    Just Ar   -> True
    Just Agua -> True
    _         -> False

--Atualiza o estado de um barril: se estiver em Ar ou Água, passa para prestes a explodir.
atualizaBarril :: Mapa -> Objeto -> Objeto
atualizaBarril mapa (Barril pos False)
  | terrenoNaoOpacoBarril mapa pos = Barril pos True
  | otherwise                      = Barril pos False
atualizaBarril _ barril = barril


--Move a bazuca na direção indicada. Se sair do mapa, é removida.
atualizaDisparoBazuca :: Mapa -> Objeto -> Either Objeto Danos
atualizaDisparoBazuca mapa (Disparo pos dir Bazuca t d) =
  let novaPos = movePosicao dir pos
  in if ePosicaoMatrizValida novaPos mapa
     then Left (Disparo novaPos dir Bazuca t d) -- Muda para a nova posição.
     else Right []  -- Removido sem explosão.
atualizaDisparoBazuca _ obj = Left obj

--Verifica se a direção é horizontal.
direcaoHorizontal :: Direcao -> Bool
direcaoHorizontal d = d == Este || d == Oeste

--Verifica se a direção é vertical.
direcaoVertical :: Direcao -> Bool
direcaoVertical d = d == Norte || d == Sul

--Move dinamite na direção horizontal e roda 45°.
dinamiteParabolica :: Posicao -> Direcao -> Maybe Ticks -> NumMinhoca -> Objeto
dinamiteParabolica pos dir t d =
  let novaPos = movePosicao dir pos
      (_, novaDir) = rodaPosicaoDirecao (pos, dir)
  in Disparo novaPos novaDir Dinamite t d

--Move dinamite para Sul e aponta para Norte.
dinamiteVertical :: Posicao -> Maybe Ticks -> NumMinhoca -> Objeto
dinamiteVertical pos t d = Disparo (movePosicao Sul pos) Norte Dinamite t d

--Atualiza a dinamite conforme a direção.
atualizaDisparoDinamite :: Mapa -> Objeto -> Objeto
atualizaDisparoDinamite mapa (Disparo pos dir Dinamite t d)
  | direcaoHorizontal dir = dinamiteParabolica pos dir t d
  | direcaoVertical dir   = dinamiteVertical pos t d
  | otherwise             = Disparo pos dir Dinamite t d
atualizaDisparoDinamite _ obj = obj

--Atualiza a posiçao da mina: se estiver em Ar ou Água, cai e aponta para Norte.
atualizaDisparoMina :: Mapa -> Objeto -> Objeto
atualizaDisparoMina mapa (Disparo (l, c) dir Mina t d)
  | terrenoNaoOpacoBarril mapa (l, c) = Disparo (l + 1, c) Norte Mina t d
  | otherwise                         = Disparo (l, c) dir Mina t d
atualizaDisparoMina _ obj = obj

--Verifica se a posição abaixo do disparo está ocupada.
disparoEstaNoChao :: Estado -> Objeto -> Bool
disparoEstaNoChao estado (Disparo (l, c) _ _ _ _) =
  let posAbaixo = (l + 1, c)
  in not (ePosicaoEstadoLivre posAbaixo estado)
disparoEstaNoChao _ _ = False

--Se for mina ou dinamite no chão, fica parado e aponta para Norte.
fixaDisparoNoChao :: Estado -> Objeto -> Objeto
fixaDisparoNoChao estado obj@(Disparo pos _ arma tempo d)
  | arma == Mina || arma == Dinamite
  , disparoEstaNoChao estado obj
  = Disparo pos Norte arma tempo d
fixaDisparoNoChao _ obj = obj

--Atualiza o tempo do objeto se for maior que 0.
atualizaTempoObjeto :: Objeto -> Objeto
atualizaTempoObjeto (Disparo pos dir arma (Just n) d)
  | n > 0     = Disparo pos dir arma (Just (n - 1)) d
  | otherwise = Disparo pos dir arma (Just 0) d
atualizaTempoObjeto obj = obj

--Verifica se a posição está dentro do raio circular.
dentroDoRaioCircular :: Posicao -> Posicao -> Int -> Bool
dentroDoRaioCircular (x, y) (i, j) r = (i - x)^2 + (j - y)^2 <= r^2

--Verifica se uma minhoca inimiga está dentro do raio circular da mina.
minhocaInimigaDentroRaio :: Posicao -> NumMinhoca -> (NumMinhoca, Minhoca) -> Bool
minhocaInimigaDentroRaio minaPos dono (i, minhoca) =
  i /= dono &&
  maybe False (\p -> dentroDoRaioCircular minaPos p 3) (posicaoMinhoca minhoca)

--Verifica se alguma minhoca inimiga está na área de explosão da mina.
existeInimigoNaExplosao :: Posicao -> NumMinhoca -> [Minhoca] -> Bool
existeInimigoNaExplosao pos dono minhocas =
  any (minhocaInimigaDentroRaio pos dono) (zip [0..] minhocas)

--Ativa mina sem tempo se houver inimigo na área de explosão.
ativaMinaSeInimigo :: Objeto -> [Minhoca] -> Objeto
ativaMinaSeInimigo obj minhocas =
  case obj of
    Disparo pos dir Mina Nothing dono
      | existeInimigoNaExplosao pos dono (filter minhocaViva minhocas)
      -> Disparo pos dir Mina (Just 2) dono
    _ -> obj

-}





