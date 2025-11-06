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

jogadaMoveLivre :: Jogada -> Posicao -> Estado -> Bool
jogadaMoveLivre (Move dir) pos estado =
  let destino = movePosicao dir pos
  in ePosicaoEstadoLivre destino estado

------------------------------------------------

validaminhocaAr :: Minhoca -> Bool
validaminhocaAr  = estanoAr
    where 
        estanoAr minhoca = case posicaoMinhoca










