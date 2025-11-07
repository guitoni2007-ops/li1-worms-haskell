{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Tarefa2
Description : Efetuar jogadas.

Módulo para a realização da Tarefa 2 de LI1\/LP1 em 2025\/26.
-}
module Tarefa2 where

import Labs2025
import Tarefa0_2025
-- | Função principal da Tarefa 2. Recebe o índice de uma minhoca na lista de minhocas, uma jogada, um estado e retorna um novo estado em que essa minhoca efetuou essa jogada.

efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n j e = undefined

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
  | not (podeEfetuarJogada estado minhoca jogada) = minhoca
  | not (podeMoverMinhoca estado minhoca jogada)  = minhoca
  | otherwise =
      let minhoca1 = mortePorForaMapa estado minhoca jogada
          minhoca2 = mortePorAgua estado minhoca1 jogada
          posValida = case posicaoMinhoca minhoca2 of
                        Just pos -> jogadaMoveLivre estado pos jogada
                        Nothing  -> False
      in if posValida then minhoca2 else minhoca2
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

-- | Cria um disparo do tipo Mina na posição de destino se estiver livre,
-- caso contrário na posição atual da minhoca, sem tempo e na direção do disparo.
disparoMina :: Estado -> Minhoca -> Jogada -> Maybe Objeto
disparoMina estado minhoca (Dispara Mina dir) =
  case posicaoMinhoca minhoca of
    Just pos ->
      case indiceMinhoca estado minhoca of
        Just dono ->
          let destino = movePosicao dir pos
              posFinal = if ePosicaoEstadoLivre destino estado then destino else pos
          in Just Disparo { posicaoDisparo = posFinal
                          , direcaoDisparo = dir
                          , tipoDisparo = Mina
                          , tempoDisparo = Nothing
                          , donoDisparo = dono }
        Nothing -> Nothing
    Nothing -> Nothing
disparoMina _ _ _ = Nothing











  

  





------------------------------------------------
--------validaminhocaAr :: Minhoca -> Bool
------------validaminhocaAr  = estanoAr
    -----where 
        -------estanoAr minhoca = case posicaoMinhoca










