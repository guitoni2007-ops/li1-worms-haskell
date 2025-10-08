module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = []

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1
