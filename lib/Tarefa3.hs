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

type Dano = Int
type Danos = [(Posicao,Dano)]

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
-- | Avança um objeto um tick: ou atualiza-o (Left Objeto) ou devolve danos de explosão (Right Danos).
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto estado _ obj =
  let
    -- verifica se o objeto já devia explodir antes de ser atualizado
    jaDeviaExplodir = deveExplodirObjeto (mapaEstado estado) obj

    -- tenta atualizar fisicamente o objeto (pode devolver Nothing se o objeto for removido/explodir)
    resultadoAtualizacao = atualizaObjetoFisica estado obj

    -- função auxiliar para construir danos a partir do objeto original
    explodirComBaseNoOriginal original =
      case posicaoObjeto original of
        Just p ->
          let raio = raioExplosao original
              posAfetadas = posicoesAfetadasPorExplosaoValida p raio (mapaEstado estado)
              danos = explosaoDanos p raio (mapaEstado estado)
          in Right danos
        Nothing -> Right []
  in
    -- Se já devia explodir antes da atualização, faz a explosão (independentemente do que atualizaObjetoFisica devolva)
    if jaDeviaExplodir
      then explodirComBaseNoOriginal obj
    else
      -- Caso contrário, olha para o resultado da atualização
      case resultadoAtualizacao of
        -- objeto atualizado com sucesso: não explode agora, fica no mapa
        Just objAtualizado -> Left objAtualizado

        -- atualização devolveu Nothing: o objeto foi removido pela atualização (ex.: bazuca saiu do mapa)
        -- neste caso, se tinha posição, consideramos que explode (com base na posição original), senão não há danos
        Nothing -> explodirComBaseNoOriginal obj





-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
-- | Aplica danos às minhocas e transforma o terreno Terra atingido em Ar
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos (Estado mapa objetos minhocas) =
  let
      -- incluir posições com dano >= 10 e que sejam Terra
      posAfetadas = [ p | (p, d) <- danos, d >= 10
                        , terrenoNaPosicao mapa p == Just Terra ]

      -- atualiza o mapa: apenas posições Terra atingidas viram Ar
      atualizaCelula l c terreno =
        if (l, c) `elem` posAfetadas then Ar else terreno

      mapa' =
        [ [ atualizaCelula l c terreno
          | (c, terreno) <- zip [0..] linha ]
        | (l, linha) <- zip [0..] mapa ]

      -- aplica dano às minhocas conforme Danos
      aplicarDano minhoca = case minhoca of
        Minhoca Nothing vida jet esc baz mina din -> minhoca

        Minhoca (Just pos) (Viva hp) jet esc baz mina din ->
          let danoTotal = sum [ d | (p, d) <- danos, p == pos ]
              novaVida  = hp - danoTotal
              estadoVida = if novaVida <= 0 then Morta else Viva novaVida
          in Minhoca (Just pos) estadoVida jet esc baz mina din

        Minhoca pos Morta jet esc baz mina din -> minhoca
  in Estado mapa' objetos (map aplicarDano minhocas)






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
 eMinhocaGravidade mapa minhoca

--Aplica gravidade à minhoca se estiver viva e sobre terreno não opaco.
minhocaComGravidade :: Mapa -> Minhoca -> Minhoca
minhocaComGravidade mapa minhoca
  | deveAplicarGravidade mapa minhoca = aplicaGravidadeMinhoca mapa minhoca
  | otherwise = minhoca

--Verifica se um objeto deve explodir imediatamente.
deveExplodirObjeto :: Mapa -> Objeto -> Bool
deveExplodirObjeto mapa obj = case obj of
  Barril _ True -> True  
  Disparo pos dir Bazuca _ _ -> not (posicaoValidaOuBazuca mapa pos dir)
  Disparo _ _ _ (Just 0) _ -> True
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

--Devolve as posições afetadas por uma explosão de diâmetro d, ignorando posições fora do mapa.
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



--Devolve a posição de um objeto, se aplicável (barris e disparos).
posicaoObjeto :: Objeto -> Maybe Posicao
posicaoObjeto (Barril p _)         = Just p
posicaoObjeto (Disparo p _ _ _ _)  = Just p


-- | Calcula o dano numa célula relativa ao centro da explosão (diâmetro d).
--   Regras:
--     - Centro: d * 10
--     - Cardinais (N/E/S/W, k==1 em eixo): (d - 2) * 10
--     - Diagonais imediatas (k==1): (d - 3) * 10
--     - Anéis seguintes (k >= 2): (d - 2*k) * 10, enquanto > 0
danoExplosao :: Int -> (Int, Int) -> Int
danoExplosao d (0,0) = d * 10
danoExplosao d (i,j)
  | k == 1 && (i == 0 || j == 0) = (d - 2) * 10
  | k == 1                       = (d - 3) * 10
  | otherwise                    = max ((d - 2 * k) * 10) 0
  where
    k = max (abs i) (abs j)

-- | Gera os danos da explosão centrada em (x,y).
--   Recorta por círculo euclidiano de raio r = d `div` 2,
--   aplica as regras de dano e inclui apenas posições válidas e com dano > 0.
explosaoDanos :: Posicao -> Int -> Mapa -> Danos
explosaoDanos (x,y) d mapa =
  [ ((x+i, y+j), dmg)
  | i <- [-r..r], j <- [-r..r]
  , i*i + j*j <= r*r
  , let dmg = danoExplosao d (i,j)
  , dmg > 0
  , ePosicaoMatrizValida (x+i, y+j) mapa
  ]
  where
    r = d `div` 2



--Transforma o terreno nas posições dadas em Ar.
transformaTerrenoEmAr :: Estado -> [Posicao] -> Estado
transformaTerrenoEmAr (Estado mapa objetos minhocas) posicoes =
  let mapa' = [ [ if (l,c) `elem` posicoes then Ar else terreno
                | (c, terreno) <- zip [0..] linha ]
              | (l, linha) <- zip [0..] mapa ]
  in Estado mapa' objetos minhocas



--Devolve o terreno na posição, se for válida.
terrenoNaPosicao :: Mapa -> Posicao -> Maybe Terreno
terrenoNaPosicao mapa (l, c)
  | ePosicaoMatrizValida (l, c) mapa = Just (mapa !! l !! c)
  | otherwise                        = Nothing

--Verifica se a posição está em terreno Ar ou Água (não opaco).
terrenoNaoOpacoBarril :: Estado -> Posicao -> Bool
terrenoNaoOpacoBarril estado (l, c) =
  case terrenoNaPosicao (mapaEstado estado) (l, c) of
    Just Ar   -> ePosicaoEstadoLivre (l + 1, c) estado
    Just Agua -> ePosicaoEstadoLivre (l + 1, c) estado
    _         -> False


--Atualiza o estado de um barril: se estiver em Ar ou Água, passa para prestes a explodir.
atualizaBarril :: Estado -> Objeto -> Objeto
atualizaBarril estado (Barril pos False)
  | terrenoNaoOpacoBarril estado pos = Barril pos True
  | otherwise                        = Barril pos False
atualizaBarril _ barril = barril




--Move a bazuca na direção indicada. Se sair do mapa, é removida.
atualizaDisparoBazuca :: Mapa -> Objeto ->  Maybe Objeto
atualizaDisparoBazuca mapa (Disparo pos dir Bazuca t d) =
  let novaPos = movePosicao dir pos
  in if ePosicaoMatrizValida novaPos mapa
     then Just (Disparo novaPos dir Bazuca t d) -- Muda para a nova posição.
     else Nothing  -- Removido sem explosão.
atualizaDisparoBazuca _ obj = Just obj -- Para outros objetos, ficam na mesma posição

--Verifica se a dinamite está “no ar”: terreno Ar e posição inferior livre
ePosicaoDinamiteLivre :: Estado -> Posicao -> Bool
ePosicaoDinamiteLivre estado (l, c) =
  case terrenoNaPosicao (mapaEstado estado) (l, c) of
    Just Ar -> ePosicaoEstadoLivre (l + 1, c) estado
    _       -> False



--Calcula as posições da dinamite no ar
rodaPosicaoDirecao1 :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao1 ((l,c),d) | d==Norte = ((l+1,c), Norte)
                             | d==Nordeste = ((l-1,c+1), Este)
                             | d==Este = ((l+1,c+1), Sudeste)
                             | d==Sudeste = ((l+1,c+1), Sul)
                             | d==Sul = ((l+1,c), Norte)
                             | d==Sudoeste = ((l+1,c-1), Sul)
                             | d==Oeste = ((l+1,c-1), Sudoeste)
                             | d==Noroeste = ((l-1,c-1), Oeste)

--Atualiza a dinamite se estiver no ar, aplicando rotação e movimento conforme rodaPosicaoDirecao1.
atualizaDinamiteRodaSeNoAr :: Estado -> Objeto -> Objeto
atualizaDinamiteRodaSeNoAr estado obj@(Disparo pos dir Dinamite t d)
  | ePosicaoDinamiteLivre estado pos =
      let (novaPos, novaDir) = rodaPosicaoDirecao1 (pos, dir)
      in if ePosicaoMatrizValida novaPos (mapaEstado estado)
         then Disparo novaPos novaDir Dinamite t d
         else obj
  | otherwise = obj
atualizaDinamiteRodaSeNoAr _ obj = obj


-- Atualiza a posição da mina: se estiver em Ar ou Água, cai uma linha para (l+1,c) e aponta para Norte.
-- Se (l+1,c) estiver fora do mapa, mantém a posição atual mas aponta para Norte.
-- Caso contrário, mantém o objeto inalterado.
atualizaDisparoMina :: Estado -> Objeto -> Objeto
atualizaDisparoMina estado obj@(Disparo (l, c) _ Mina t d) =
  case terrenoNaPosicao (mapaEstado estado) (l, c) of
    Just Ar  -> moverOuApontar (l + 1, c)
    Just Agua -> moverOuApontar (l + 1, c)
    _        -> obj
  where
    moverOuApontar destino =
      if ePosicaoMatrizValida destino (mapaEstado estado)
        then Disparo destino Norte Mina t d
        else Disparo (l, c) Norte Mina t d
atualizaDisparoMina _ obj = obj

--Verifica se a posição abaixo do disparo está ocupada.
disparoEstaNoChao :: Estado -> Objeto -> Bool
disparoEstaNoChao estado (Disparo (l, c) _ _ _ _) =
  let posAbaixo = (l + 1, c)
  in not (ePosicaoEstadoLivre posAbaixo estado)
disparoEstaNoChao _ _ = False

-- Se for mina ou dinamite no chão, fica parado e aponta para Norte.
-- (Apenas trata o caso de “no chão”; comportamento em Ar/Água é tratado nas funções específicas.)
fixaDisparoNoChao :: Estado -> Objeto -> Objeto
fixaDisparoNoChao estado obj@(Disparo (l, c) _ arma tempo dono)
  | (arma == Mina || arma == Dinamite)
  , disparoEstaNoChao estado obj
  = Disparo (l, c) Norte arma tempo dono
  | otherwise = obj



--Atualiza o tempo do objeto se for maior que 0.
atualizaTempoObjeto :: Objeto -> Objeto
atualizaTempoObjeto (Disparo pos dir arma (Just n) d)
  | n > 0     = Disparo pos dir arma (Just (n - 1)) d
  | otherwise = Disparo pos dir arma (Just 0) d
atualizaTempoObjeto obj = obj

--Verifica se a minhoca é inimiga e está viva.
minhocaInimigaViva :: Int -> Minhoca -> Bool
minhocaInimigaViva dono (Minhoca _ vida num _ _ _ _) =
  num /= dono && minhocaViva (Minhoca Nothing vida num 0 0 0 0)

--Verifica se a minhoca está na área de explosão da mina.
minhocaNaAreaExplosao :: Posicao -> Mapa -> Minhoca -> Bool
minhocaNaAreaExplosao centro mapa (Minhoca (Just pos) _ _ _ _ _ _) =
  pos `elem` posicoesAfetadasPorExplosaoValida centro 3 mapa
minhocaNaAreaExplosao _ _ _ = False

--Verifica se alguma minhoca inimiga viva está na área de explosão da mina.
existeInimigoNaArea :: Posicao -> Int -> Mapa -> [Minhoca] -> Bool
existeInimigoNaArea centro dono mapa minhocas =
  any (\m -> minhocaInimigaViva dono m && minhocaNaAreaExplosao centro mapa m) minhocas

--Ativa mina sem tempo se houver inimigo na área.
ativaMinaSeInimigo :: Mapa -> [Minhoca] -> Objeto -> Objeto
ativaMinaSeInimigo mapa minhocas obj@(Disparo pos _ Mina Nothing dono)
  | existeInimigoNaArea pos dono mapa minhocas = Disparo pos Norte Mina (Just 2) dono
  | otherwise = obj
ativaMinaSeInimigo _ _ obj = obj

atualizaObjetoFisica :: Estado -> Objeto -> Maybe Objeto
atualizaObjetoFisica estado obj = case obj of
  -- 1. Barril -> atualiza primeiro, depois verifica se explode
  Barril p estadoAntes ->
    let barrilAtualizado@(Barril _ estadoDepois) = atualizaBarril estado (Barril p estadoAntes)
    in case (estadoAntes, estadoDepois) of
         (True, True)  -> Nothing
         (False, True) -> Just barrilAtualizado
         _             -> Just barrilAtualizado

  -- 2. Bazuca -> avança; se sair do mapa, é removida
  Disparo p d Bazuca t dono ->
    atualizaDisparoBazuca (mapaEstado estado) (Disparo p d Bazuca t dono)

  -- 3. Mina sem tempo -> primeiro aplica movimento/gravidade (se aplicável),
  --    depois verifica ativação com base na nova posição e nas minhocas já atualizadas.
  Disparo p@(l, c) d Mina Nothing dono ->
    let
      destino = (l + 1, c)

      moverOuApontar dest =
        if ePosicaoMatrizValida dest (mapaEstado estado)
          then Disparo dest Norte Mina Nothing dono
          else Disparo p Norte Mina Nothing dono

      moved =
        case terrenoNaPosicao (mapaEstado estado) p of
          Just Ar   -> moverOuApontar destino
          Just Agua -> moverOuApontar destino
          Just Terra ->
            if ePosicaoMatrizValida destino (mapaEstado estado) && ePosicaoEstadoLivre destino estado
              then Disparo destino Norte Mina Nothing dono
              else Disparo p d Mina Nothing dono
          _ -> Disparo p d Mina Nothing dono
    in
      case posicaoObjeto moved of
        Just posMoved ->
          if existeInimigoNaArea posMoved dono (mapaEstado estado) (minhocasEstado estado)
            then Just (Disparo posMoved Norte Mina (Just 2) dono) -- ativada na nova posição
            else
              -- se a mina não se moveu (permanece na mesma posição), tenta a ativação por inimigo
              if posMoved == p
                then Just (ativaMinaSeInimigo (mapaEstado estado) (minhocasEstado estado) (Disparo p d Mina Nothing dono))
                else Just moved
        Nothing -> Just moved

  -- 4. Mina com tempo -> aplica comportamento de queda/rotação e decrementa tempo
  Disparo p d Mina (Just tt) dono ->
    Just (atualizaTempoObjeto (atualizaDisparoMina estado (Disparo p d Mina (Just tt) dono)))

  -- 5. Mina ou Dinamite no chão -> fixa e aponta para Norte
  Disparo p d arma t dono
    | (arma == Mina || arma == Dinamite)
    , disparoEstaNoChao estado (Disparo p d arma t dono) ->
        Just (atualizaTempoObjeto (fixaDisparoNoChao estado (Disparo p d arma t dono)))

  -- 6. Dinamite -> comportamento depende do ambiente; se estiver em Ar/Água, mantém mas vai explodir
  Disparo p d Dinamite t dono ->
    let objAtualizado = atualizaDinamiteRodaSeNoAr estado (Disparo p d Dinamite t dono)
    in Just (atualizaTempoObjeto objAtualizado)

  -- 7. Caso geral -> mantém
  _ -> Just obj





