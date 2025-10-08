module Main where

import Labs2025
import Tarefa3
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 3
testesTarefa3 :: [Estado]
testesTarefa3 = []

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3