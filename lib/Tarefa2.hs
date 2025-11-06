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
  in ePosicaoEstadoLivre destino estado

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

mortePorForaMapa :: Estado -> Minhoca -> Jogada -> Minhoca
mortePorForaMapa estado minhoca (Move dir) =
  case posicaoMinhoca minhoca of
    Nothing -> minhoca  -- já está sem posição
    Just pos ->
      let destino = movePosicao dir pos
          mapa = mapaEstado estado
      in if ePosicaoMatrizValida destino mapa
         then minhoca  -- posição válida, continua viva
         else minhoca { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
mortePorForaMapa _ minhoca _ = minhoca  -- só interessa a jogada Move












------------------------------------------------
--------validaminhocaAr :: Minhoca -> Bool
------------validaminhocaAr  = estanoAr
    -----where 
        -------estanoAr minhoca = case posicaoMinhoca










