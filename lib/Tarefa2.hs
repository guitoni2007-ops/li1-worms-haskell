{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Tarefa0_2025
import Data.Maybe (fromMaybe)
---verifica se a minhoca esta viva
validaminhocaviva :: [Minhoca] -> Bool
validaminhocaviva = all estaViva
  where
    estaViva minhoca = case vidaMinhoca minhoca of
      Viva n -> n > 0
      Morta  -> False
-- | verifica se o destino da jogada esta livre
jogadaMoveLivre :: Estado -> Posicao -> Jogada -> Bool
jogadaMoveLivre estado pos (Move dir) =
  let destino = movePosicao dir pos
      mapa = mapaEstado estado
  in ePosicaoMatrizValida destino mapa && ePosicaoEstadoLivre destino estado

-- | verifica se a minhoca esta no chao
estaNoChao :: Estado -> Minhoca -> Bool
estaNoChao estado minhoca =
  case posicaoMinhoca minhoca of
    Nothing -> False
    Just (l, c) ->
      let posAbaixo = (l + 1, c)
      in not (ePosicaoEstadoLivre posAbaixo estado)

-- | verifica se a minhoca pode saltar (estando no chao)
podeEfetuarJogada :: Estado -> Minhoca -> Jogada -> Bool
podeEfetuarJogada estado minhoca (Move dir)
  | dir `elem` [Norte, Nordeste, Noroeste] = estaNoChao estado minhoca
  | otherwise = True  -- ainda não verificamos as outras direções
podeEfetuarJogada _ _ _ = True

-- | verifica se uma minhoca se pode movimentar
podeMoverMinhoca :: Estado -> Minhoca -> Jogada -> Bool
podeMoverMinhoca estado minhoca (Move _) = estaNoChao estado minhoca

-- | mata uma minhoca dando lhe a posição nothing
matarMinhoca :: Minhoca -> Minhoca
matarMinhoca m = m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }

-- | mata uma minhoca por sair fora do mapa
mortePorForaMapa :: Estado -> Minhoca -> Jogada -> Minhoca
mortePorForaMapa estado minhoca (Move dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      let destino = movePosicao dir pos
          mapa = mapaEstado estado
      in if ePosicaoMatrizValida destino mapa
         then minhoca
         else matarMinhoca minhoca
    Nothing -> minhoca
mortePorForaMapa _ minhoca _ = minhoca

-- | mata uma minhoca por ir para a água
mortePorAgua :: Estado -> Minhoca -> Jogada -> Minhoca
mortePorAgua estado minhoca (Move dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      let destino = movePosicao dir pos
      in if ePosicaoMatrizValida destino (mapaEstado estado)
         then case encontraPosicaoMatriz destino (mapaEstado estado) of
                Just Agua -> minhoca { posicaoMinhoca = Just destino, vidaMinhoca = Morta }
                _         -> minhoca
         else minhoca  -- posição fora do mapa → não faz nada ou já morre noutro lugar
    Nothing -> minhoca

-- | verifica se a jogada move é valida compilando as verificações
verificaJogadaMov :: Estado -> Minhoca -> Jogada -> Minhoca
verificaJogadaMov estado minhoca jogada@(Move dir)
  -- Minhoca morta não se mexe
  | vidaMinhoca minhoca == Morta = minhoca

  -- Não pode efetuar jogada (ex: está no ar e tenta subir)
  | not (podeEfetuarJogada estado minhoca jogada) = minhoca

  -- Não pode mover (ex: não está no chão e tenta mover)
  | not (podeMoverMinhoca estado minhoca jogada)  = minhoca

  | otherwise =
      case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just pos ->
          let destino = movePosicao dir pos
              mapa = mapaEstado estado

              -- Aplica mortes por sair do mapa ou cair na água
              minhoca1 = mortePorForaMapa estado minhoca jogada
              minhoca2 = mortePorAgua estado minhoca1 jogada

          in case posicaoMinhoca minhoca2 of
               Nothing -> minhoca2  -- morreu ou inválida
               Just _ ->
                 if ePosicaoMatrizValida destino mapa
                    && ePosicaoEstadoLivre destino estado
                 then minhoca2 { posicaoMinhoca = Just destino }
                 else minhoca2

verificaJogadaMov _ minhoca _ = minhoca

----------------------------Disparos------------------------------------------------------------------
-- | Verifica se a minhoca pode disparar a arma e atualiza a munição caso possa.

verificaJogadaDisparo :: Minhoca -> TipoArma -> Maybe Minhoca
verificaJogadaDisparo minhoca arma
  | vidaMinhoca minhoca == Morta = Nothing  -- não pode disparar se estiver morta
  | encontraQuantidadeArmaMinhoca arma minhoca <= 0 = Nothing  -- sem munição
  | otherwise = Just (disparaArma arma minhoca)  -- decrementa a munição

-- | Verifica se a minhoca pode disparar a arma, ou seja,
-- não existe um disparo ativo da mesma arma pertencente a ela.
podeDispararMesmoTipo :: TipoArma -> NumMinhoca -> Estado -> Bool
podeDispararMesmoTipo arma numMinhoca estado =
  not (minhocaTemDisparo arma numMinhoca (objetosEstado estado))

-- | verifica se a posição encontra se ocupada por entidades ou se é do tipo Terra ou Pedra
ePosicaoBloqueada :: Posicao -> Estado -> Bool
ePosicaoBloqueada pos estado =
  let mapa = mapaEstado estado
      terreno = encontraPosicaoMatriz pos mapa
  in case terreno of
       Just Terra -> True
       Just Pedra -> True
       _          -> ePosicaoOcupadaPorEntidade pos estado -- Verifica outras entidades

-- | verifica se a minhoca pode disparar o jetpack
podeDisparoJetpack :: Estado -> Minhoca -> Jogada -> Bool
podeDisparoJetpack estado minhoca (Dispara Jetpack dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      let destino = movePosicao dir pos
      in not (ePosicaoBloqueada destino estado) 
    Nothing -> False
podeDisparoJetpack _ _ _ = False

-- | Retorna o índice de uma minhoca no estado, se existir.
indiceMinhoca :: Estado -> Minhoca -> Maybe NumMinhoca
indiceMinhoca estado minhoca = aux 0 (minhocasEstado estado)
  where
    aux _ [] = Nothing
    aux i (m:ms)
      | m == minhoca = Just i
      | otherwise    = aux (i+1) ms

-- | Cria um disparo de Bazuca na posição de destino da jogada, se a minhoca existir no estado.
disparoBazuca :: Estado -> Minhoca -> Jogada -> Maybe Objeto
disparoBazuca estado minhoca (Dispara Bazuca dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      case indiceMinhoca estado minhoca of
        Just dono ->
          let destino = movePosicao dir pos
          in Just Disparo { posicaoDisparo = destino
                          , direcaoDisparo = dir
                          , tipoDisparo = Bazuca
                          , tempoDisparo = Nothing
                          , donoDisparo = dono }
        Nothing -> Nothing
    Nothing -> Nothing
disparoBazuca _ _ _ = Nothing

-- | Verifica se uma posição está ocupada por uma minhoca ou barril.
-- Não verifica se a posição está dentro do mapa.
ePosicaoOcupadaPorEntidade :: Posicao -> Estado -> Bool
ePosicaoOcupadaPorEntidade pos (Estado _ objetos minhocas) =
  any (\m -> posicaoMinhoca m == Just pos) minhocas ||
  any (\o -> case o of
               Barril p _ -> p == pos
               _ -> False) objetos


-- Disparo de Mina
disparoMina :: Estado -> Minhoca -> Jogada -> Maybe Objeto
disparoMina estado minhoca (Dispara Mina dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      case indiceMinhoca estado minhoca of
        Just dono ->
          let destino = movePosicao dir pos
              -- posiçao bloqueada Terra Pedra ou Entidade
              bloqueada = ePosicaoBloqueada destino estado
              -- Se bloqueada, fica na posição atual (pos), senão vai para o destino
              posFinal = if bloqueada then pos else destino
          in Just Disparo
              { posicaoDisparo = posFinal
              , direcaoDisparo = dir
              , tipoDisparo = Mina
              , tempoDisparo = Nothing
              , donoDisparo = dono
              }
        Nothing -> Nothing
    Nothing -> Nothing
disparoMina _ _ _ = Nothing


-- | Cria um disparo do tipo Dinamite na posição de destino se estiver livre,
-- caso contrário na posição atual da minhoca, com tempo 4 e na direção do disparo.
-- Disparo de Dinamite
disparoDinamite :: Estado -> Minhoca -> Jogada -> Maybe Objeto
disparoDinamite estado minhoca (Dispara Dinamite dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      case indiceMinhoca estado minhoca of
        Just dono ->
          let destino = movePosicao dir pos
              -- Posição Bloqueada (Terra, Pedra, Entidade)
              bloqueada = ePosicaoBloqueada destino estado
              -- Se bloqueada, fica na posição atual (pos), senão vai para o destino
              posFinal = if bloqueada then pos else destino
          in Just Disparo
              { posicaoDisparo = posFinal
              , direcaoDisparo = dir
              , tipoDisparo = Dinamite
              , tempoDisparo = Just 4 
              , donoDisparo = dono
              }
        Nothing -> Nothing
    Nothing -> Nothing
disparoDinamite _ _ _ = Nothing

-- | Adiciona barris ou disparos dentro do mapa
adicionaObjetoSeDentroMapa :: Estado -> Objeto -> Estado
adicionaObjetoSeDentroMapa estado obj =
    let mapa = mapaEstado estado
        pos = case obj of
                Disparo { posicaoDisparo = p } -> p
                Barril { posicaoBarril = p }   -> p
    in if ePosicaoMatrizValida pos mapa
       then estado { objetosEstado = objetosEstado estado ++ [obj] }
       else estado  -- fora do mapa → objeto eliminado

-- | Adiciona um disparo ao estado
adicionaDisparoAoEstado :: Estado -> Maybe Objeto -> Estado
adicionaDisparoAoEstado estado (Just disparo) = adicionaObjetoSeDentroMapa estado disparo
adicionaDisparoAoEstado estado Nothing = estado

-- | Atualiza uma minhoca específica dentro do estado.
atualizaMinhocaNoEstado :: Estado -> Minhoca -> Minhoca -> [Minhoca]
atualizaMinhocaNoEstado estado minhocaAntiga minhocaNova =
  map (\m -> if m == minhocaAntiga then minhocaNova else m) (minhocasEstado estado)

-- | Verifica se a minhoca pode disparar a arma (agrupa todas as validações iniciais)
podeDispararArma :: Estado -> Minhoca -> TipoArma -> Bool
podeDispararArma estado minhoca arma =
  vidaMinhoca minhoca /= Morta &&
  encontraQuantidadeArmaMinhoca arma minhoca > 0 &&
  podeDispararMesmoTipo arma (fromMaybe (-1) (indiceMinhoca estado minhoca)) estado


-- | Processa o disparo de acordo com o tipo de arma
processaDisparoPorTipoArma :: Estado -> Minhoca -> Minhoca -> Jogada -> TipoArma -> Estado
processaDisparoPorTipoArma estado minhoca minhocaComMenosMunicao jogada arma =
  case arma of
    Jetpack    -> processaJetpack estado minhoca minhocaComMenosMunicao jogada
    Escavadora -> processaEscavadora estado minhoca minhocaComMenosMunicao jogada
    Bazuca     -> processaArmaExplosiva estado minhoca minhocaComMenosMunicao jogada disparoBazuca
    Mina       -> processaArmaExplosiva estado minhoca minhocaComMenosMunicao jogada disparoMina
    Dinamite   -> processaArmaExplosiva estado minhoca minhocaComMenosMunicao jogada disparoDinamite

-- | Gasta munição ao disparar
gastaMunicaoJetpack :: Estado -> Minhoca -> Minhoca -> Estado
gastaMunicaoJetpack estado minhoca minhocaComMenosMunicao =
  let minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaComMenosMunicao
  in estado { minhocasEstado = minhocasNovas }

-- | Processa o disparo de Jetpack
processaJetpack :: Estado -> Minhoca -> Minhoca -> Jogada -> Estado
processaJetpack estado minhoca minhocaComMenosMunicao jogada@(Dispara Jetpack dir) =
  let Just pos = posicaoMinhoca minhoca
      destino = movePosicao dir pos
      mapa = mapaEstado estado
      destinoValido = ePosicaoMatrizValida destino mapa && not (ePosicaoBloqueada destino estado) -- Verifica se é válido E não bloqueado
  in if destinoValido
     then moveComJetpack estado minhoca minhocaComMenosMunicao destino -- Move normalmente
     else if ePosicaoMatrizValida destino mapa -- Se for inválido, mas dentro do mapa (bloqueado por Terra/Entidade)
          then gastaMunicaoJetpack estado minhoca minhocaComMenosMunicao -- Apenas gasta munição
          else mataComJetpack estado minhoca minhocaComMenosMunicao -- Fora do mapa (morre)



-- | Move a minhoca com Jetpack para um destino válido
moveComJetpack :: Estado -> Minhoca -> Minhoca -> Posicao -> Estado
moveComJetpack estado minhoca minhocaComMenosMunicao destino =
  let minhocaNova = minhocaComMenosMunicao { posicaoMinhoca = Just destino }
      minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
  in estado { minhocasEstado = minhocasNovas }


-- | Mata a minhoca quando o Jetpack a leva para fora do mapa
mataComJetpack :: Estado -> Minhoca -> Minhoca -> Estado
mataComJetpack estado minhoca minhocaComMenosMunicao =
  let minhocaMorta = minhocaComMenosMunicao
                       { vidaMinhoca = Morta
                       , posicaoMinhoca = Nothing }
      minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaMorta
  in estado { minhocasEstado = minhocasNovas }

-- | Processa o disparo de Escavadora
processaEscavadora :: Estado -> Minhoca -> Minhoca -> Jogada -> Estado
processaEscavadora estado minhoca minhocaComMenosMunicao jogada@(Dispara Escavadora dir) =
  let Just pos = posicaoMinhoca minhoca
      destino = movePosicao dir pos
      mapaAtual = mapaEstado estado
      terrenoDestino = encontraPosicaoMatriz destino mapaAtual
  in if not (ePosicaoMatrizValida destino mapaAtual)
     then gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao -- Fora do mapa: gasta munição, não move
     else if ePosicaoOcupadaPorEntidade destino estado -- Destino ocupado por outra entidade: gasta munição, não move
          then gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao
          else  if not (estaNoChao estado minhoca)
                  then gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao
                  else case terrenoDestino of
         Just Terra -> escavaEMove estado minhoca minhocaComMenosMunicao destino dir
         Just Pedra -> gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao
         Just Agua  -> gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao
         Just Ar    -> moveEscavadora estado minhoca minhocaComMenosMunicao destino
         _          -> gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao

processaEscavadora estado _ _ _ = estado

-- | Minhoca move-se com a escavadora
moveEscavadora :: Estado -> Minhoca -> Minhoca -> Posicao -> Estado
moveEscavadora estado minhoca minhocaComMenosMunicao destino =
  let minhocaNova = minhocaComMenosMunicao { posicaoMinhoca = Just destino }
      minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
  in estado { minhocasEstado = minhocasNovas }

-- | Gasta muniçao ao disparar 
gastaMunicaoEscavadora :: Estado -> Minhoca -> Minhoca -> Estado
gastaMunicaoEscavadora estado minhoca minhocaComMenosMunicao =
  let minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaComMenosMunicao
  in estado { minhocasEstado = minhocasNovas }

-- | Minhoca transforma a terra em ar e move se 
escavaEMove :: Estado -> Minhoca -> Minhoca -> Posicao -> Direcao -> Estado
escavaEMove estado minhoca minhocaComMenosMunicao destino dir =
  let mapaAtual = mapaEstado estado
      mapaNovo = destroiPosicao destino mapaAtual
      minhocaNova = minhocaComMenosMunicao { posicaoMinhoca = Just destino }
      minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
  in estado { mapaEstado = mapaNovo, minhocasEstado = minhocasNovas }

-- | Processa armas explosivas (Bazuca, Mina, Dinamite)
processaArmaExplosiva :: Estado -> Minhoca -> Minhoca -> Jogada 
                      -> (Estado -> Minhoca -> Jogada -> Maybe Objeto) 
                      -> Estado
processaArmaExplosiva estado minhoca minhocaComMenosMunicao jogada criaDisparo =
  let disparo = criaDisparo estado minhoca jogada
      estComDisparo = adicionaDisparoAoEstado estado disparo
      minhocasNovas = atualizaMinhocaNoEstado estComDisparo minhoca minhocaComMenosMunicao
  in estComDisparo { minhocasEstado = minhocasNovas }

-- | Verifica se a jogada do tipo disparo é válida compilando todas as verificações anteriores
verificaJogadaDisparoCompleta :: Estado -> Minhoca -> Jogada -> Estado
verificaJogadaDisparoCompleta estado minhoca jogada@(Dispara arma dir)
  | not (podeDispararArma estado minhoca arma) = estado
  | otherwise =
      case verificaJogadaDisparo minhoca arma of
        Nothing -> estado
        Just minhocaComMenosMunicao ->
          processaDisparoPorTipoArma estado minhoca minhocaComMenosMunicao jogada arma
verificaJogadaDisparoCompleta estado _ _ = estado

-- | Efetua a jogada escolhida se for válida
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada numMinhoca jogada estado =
  case encontraIndiceLista numMinhoca (minhocasEstado estado) of
    Nothing -> estado  -- índice inválido
    Just minhoca ->
      case jogada of
        Move _ ->
          let minhocaNova = verificaJogadaMov estado minhoca jogada
              minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
          in estado { minhocasEstado = minhocasNovas }

        Dispara _ _ ->
          verificaJogadaDisparoCompleta estado minhoca jogada









  





