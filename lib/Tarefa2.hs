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

-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.



validaminhocaviva :: [Minhoca] -> Bool
validaminhocaviva = all estaViva
  where
    estaViva minhoca = case vidaMinhoca minhoca of
      Viva n -> n > 0
      Morta  -> False

jogadaMoveLivre :: Estado -> Posicao -> Jogada -> Bool
jogadaMoveLivre estado pos (Move dir) =
  let destino = movePosicao dir pos
      mapa = mapaEstado estado
  in ePosicaoMatrizValida destino mapa && ePosicaoEstadoLivre destino estado


estaNoChao :: Estado -> Minhoca -> Bool
estaNoChao estado minhoca =
  case posicaoMinhoca minhoca of
    Nothing -> False
    Just (x, y) ->
      let posAbaixo = (x, y + 1)
      in not (ePosicaoEstadoLivre posAbaixo estado)

podeEfetuarJogada :: Estado -> Minhoca -> Jogada -> Bool
podeEfetuarJogada estado minhoca (Move dir)
  | dir `elem` [Norte, Nordeste, Noroeste] = estaNoChao estado minhoca
  | otherwise = True  -- ainda não verificamos as outras direções
podeEfetuarJogada _ _ _ = True

podeMoverMinhoca :: Estado -> Minhoca -> Jogada -> Bool
podeMoverMinhoca estado minhoca (Move _) = estaNoChao estado minhoca

matarMinhoca :: Minhoca -> Minhoca
matarMinhoca m = m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }

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

-- | Verifica se a minhoca pode usar um disparo do tipo Jetpack para se mover numa direção
podeDisparoJetpack :: Estado -> Minhoca -> Jogada -> Bool
podeDisparoJetpack estado minhoca (Dispara Jetpack dir) =
  case posicaoMinhoca minhoca of
    Just pos -> ePosicaoEstadoLivre (movePosicao dir pos) estado
    Nothing  -> False
podeDisparoJetpack _ _ _ = False

-- | Aplica o efeito da Escavadora: destrói Terra e retorna a nova minhoca e o mapa atualizado
disparoEscavadoraMinhoca :: Estado -> Minhoca -> Jogada -> (Mapa, Minhoca)
disparoEscavadoraMinhoca estado minhoca jogada@(Dispara Escavadora dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      let destino = movePosicao dir pos
          mapaAtual = mapaEstado estado
      in case encontraPosicaoMatriz destino mapaAtual of
           Just Terra ->
             let mapaNovo = destroiPosicao destino mapaAtual
                 minhocaNova = minhoca { posicaoMinhoca = Just destino }
             in (mapaNovo, minhocaNova)
           _ ->
             -- se não houver Terra, minhoca não se move
             (mapaAtual, minhoca)
    Nothing -> (mapaEstado estado, minhoca)  -- minhoca sem posição permanece igual
disparoEscavadoraMinhoca estado minhoca _ = (mapaEstado estado, minhoca)

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
              ocupada = ePosicaoOcupadaPorEntidade destino estado
              posFinal = if ocupada then pos else destino
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
              ocupada = ePosicaoOcupadaPorEntidade destino estado
              posFinal = if ocupada then pos else destino
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

adicionaObjetoSeDentroMapa :: Estado -> Objeto -> Estado
adicionaObjetoSeDentroMapa estado obj =
    let mapa = mapaEstado estado
        pos = case obj of
                Disparo { posicaoDisparo = p } -> p
                Barril { posicaoBarril = p }   -> p
    in if ePosicaoMatrizValida pos mapa
       then estado { objetosEstado = objetosEstado estado ++ [obj] }
       else estado  -- fora do mapa → objeto eliminado

adicionaDisparoAoEstado :: Estado -> Maybe Objeto -> Estado
adicionaDisparoAoEstado estado (Just disparo) = adicionaObjetoSeDentroMapa estado disparo
adicionaDisparoAoEstado estado Nothing = estado

-- | Atualiza uma minhoca específica dentro do estado.
atualizaMinhocaNoEstado :: Estado -> Minhoca -> Minhoca -> [Minhoca]
atualizaMinhocaNoEstado estado minhocaAntiga minhocaNova =
  map (\m -> if m == minhocaAntiga then minhocaNova else m) (minhocasEstado estado)

verificaJogadaDisparoCompleta :: Estado -> Minhoca -> Jogada -> Estado
verificaJogadaDisparoCompleta estado minhoca jogada@(Dispara arma dir)
  | vidaMinhoca minhoca == Morta = estado
  | encontraQuantidadeArmaMinhoca arma minhoca <= 0 = estado
  | not (podeDispararMesmoTipo arma (fromMaybe (-1) (indiceMinhoca estado minhoca)) estado) = estado
  | otherwise =
      case verificaJogadaDisparo minhoca arma of
        Nothing -> estado
        Just minhocaComMenosMunicao ->
          case arma of

            -- Jetpack: movimenta-se se o destino estiver livre
            Jetpack ->
              if podeDisparoJetpack estado minhoca jogada
                then
                  let Just pos = posicaoMinhoca minhoca
                      destino = movePosicao dir pos
                      minhocaNova = minhocaComMenosMunicao { posicaoMinhoca = Just destino }
                      minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
                  in estado { minhocasEstado = minhocasNovas }
                else estado

            -- Escavadora: destrói Terra e move
            Escavadora ->
              let (mapaNovo, minhocaNova) = disparoEscavadoraMinhoca estado minhoca jogada
                  minhocasNovas = atualizaMinhocaNoEstado estado minhoca minhocaNova
              in estado { mapaEstado = mapaNovo, minhocasEstado = minhocasNovas }

            -- Bazuca, Mina, Dinamite criam disparos
            Bazuca   -> adicionaDisparo estado minhoca minhocaComMenosMunicao jogada disparoBazuca
            Mina     -> adicionaDisparo estado minhoca minhocaComMenosMunicao jogada disparoMina
            Dinamite -> adicionaDisparo estado minhoca minhocaComMenosMunicao jogada disparoDinamite
  where
    adicionaDisparo est mAnt mNova jog f =
      let disparo = f est mAnt jog
          estComDisparo = adicionaDisparoAoEstado est disparo
          minhocasNovas = atualizaMinhocaNoEstado estComDisparo mAnt mNova
      in estComDisparo { minhocasEstado = minhocasNovas }

verificaJogadaDisparoCompleta estado _ _ = estado


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









  





