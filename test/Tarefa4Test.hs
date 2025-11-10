module Main where

import Labs2025
import Tarefa4
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 = []

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4