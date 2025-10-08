module Main where

import Labs2025
import Tarefa2
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 2
testesTarefa2 :: [(NumMinhoca,Jogada,Estado)]
testesTarefa2 = []

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
