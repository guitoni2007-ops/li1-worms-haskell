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


mapaTesteValido15 =
  [ [Agua, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Agua, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

objetosTeste15 =
  [Barril (2,2) True
  ,Disparo (1,2) Sul Bazuca Nothing 5 ]


objetosTeste16 =
  ( Disparo (1,2) Norte Bazuca (Just 0) 5 )

e1 = Estado mapaTesteValido15 objetosTeste15 minhocasTeste1



minhocasTeste1 =
  [ Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1 ]



















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
avancaObjeto estado _ obj =
  case atualizaObjetoFisica estado obj of
    Just objAtualizado ->
      if deveExplodirObjeto (mapaEstado estado) obj
        then case posicaoObjeto objAtualizado of
               Just p -> Right (calculaDanosExplosao p (raioExplosao objAtualizado) (mapaEstado estado))
               Nothing -> Right []
        else Left objAtualizado

    Nothing ->
      case posicaoObjeto obj of
        Just p -> Right (calculaDanosExplosao p (raioExplosao obj) (mapaEstado estado))
        Nothing -> Right []



-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos (Estado mapa objetos minhocas) =
  let aplicarDano minhoca = case minhoca of
        Minhoca Nothing vida jet esc baz mina din ->
          Minhoca Nothing vida jet esc baz mina din

        Minhoca (Just pos) (Viva hp) jet esc baz mina din ->
          let danoTotal = sum [d | (p, d) <- danos, p == pos]
              novaVida = hp - danoTotal
              estadoVida = if novaVida <= 0 then Morta else Viva novaVida
          in Minhoca (Just pos) estadoVida jet esc baz mina din

        Minhoca pos Morta jet esc baz mina din ->
          Minhoca pos Morta jet esc baz mina din
  in Estado mapa objetos (map aplicarDano minhocas)


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

--Gera danos de explosão numa área válida do mapa.
calculaDanosExplosao :: Posicao -> Int -> Mapa -> Danos
calculaDanosExplosao (x, y) d mapa =
  [((x+i, y+j), danoExplosao d (i,j)) | i <- [-r..r], j <- [-r..r], danoExplosao d (i,j) > 0, ePosicaoMatrizValida (x+i, y+j) mapa]
  where r = d `div` 2

    
--Calcula o dano causado numa célula relativa à posição central da explosão.
danoExplosao :: Int -> (Int, Int) -> Int
danoExplosao d (0,0) = d * 10
danoExplosao d (i,j)
  | abs i + abs j == 1         = (d - 2) * 10
  | abs i == 1 && abs j == 1   = (d - 3) * 10
  | otherwise =
      let dist = abs i + abs j
      in max ((d - dist) * 10) 0



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


--Atualiza a posição da mina: se estiver em Ar ou Água, cai e aponta para Norte.
atualizaDisparoMina :: Estado -> Objeto -> Objeto
atualizaDisparoMina estado obj@(Disparo (l, c) _ Mina t d)
  | Just terreno <- terrenoNaPosicao (mapaEstado estado) (l, c)
  , terreno == Ar || terreno == Agua
  , ePosicaoEstadoLivre (l + 1, c) estado =
      Disparo (l + 1, c) Norte Mina t d
  | otherwise = obj


--Verifica se a posição abaixo do disparo está ocupada.
disparoEstaNoChao :: Estado -> Objeto -> Bool
disparoEstaNoChao estado (Disparo (l, c) _ _ _ _) =
  let posAbaixo = (l + 1, c)
  in not (ePosicaoEstadoLivre posAbaixo estado)
disparoEstaNoChao _ _ = False


--Se for mina ou dinamite no chão, fica parado e aponta para Norte.
fixaDisparoNoChao :: Estado -> Objeto -> Objeto
fixaDisparoNoChao estado obj@(Disparo (l, c) _ arma tempo dono)
  -- Caso 1: Mina no ar ou na água -> cai verticalmente e aponta para Norte
  | arma == Mina
  , terrenoNaoOpacoBarril estado (l, c)
  = Disparo (l + 1, c) Norte Mina tempo dono

  -- Caso 2: Mina ou Dinamite no chão -> fica parado e aponta para Norte
  | (arma == Mina || arma == Dinamite)
  , disparoEstaNoChao estado obj
  = Disparo (l, c) Norte arma tempo dono

  -- Caso geral: mantém
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

--Atualiza a posição de todos os objetos que não sofrem explosão
atualizaObjetoFisica :: Estado -> Objeto -> Maybe Objeto
atualizaObjetoFisica estado obj = case obj of
  -- 1. Barril -> atualiza primeiro, depois verifica se explode
  Barril _ _->
    let barrilAtualizado = atualizaBarril estado obj
    in if deveExplodirObjeto (mapaEstado estado) barrilAtualizado
       then Nothing
       else Just barrilAtualizado

  -- 2. Bazuca -> avança; se sair do mapa, é removida
  Disparo _ _ Bazuca _ _ ->
    atualizaDisparoBazuca (mapaEstado estado) obj

  -- 3. Mina sem tempo -> ativa se houver inimigo na área
  Disparo _ _ Mina Nothing _ ->
    Just (ativaMinaSeInimigo (mapaEstado estado) (minhocasEstado estado) obj)

  -- 4. Mina com tempo -> cai se estiver no ar ou água
  Disparo _ _ Mina (Just _) _ ->
    Just (atualizaTempoObjeto (atualizaDisparoMina estado obj))

  -- 5. Mina ou Dinamite no chão -> fixa e aponta para Norte
  Disparo _ _ arma _ _
    | (arma == Mina || arma == Dinamite)
    , disparoEstaNoChao estado obj ->
        Just (atualizaTempoObjeto (fixaDisparoNoChao estado obj))

  -- 6. Dinamite -> comportamento depende do ambiente
  Disparo _ _ Dinamite _ _ ->
    Just (atualizaTempoObjeto (atualizaDinamiteRodaSeNoAr estado obj))

  -- 7. Caso geral -> mantém
  _ -> Just obj





