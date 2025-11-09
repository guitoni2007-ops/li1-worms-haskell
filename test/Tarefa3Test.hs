module Main where

import Labs2025
import Tarefa3
import Magic
{-
cabal clean && rm -rf t3-feedback.tix
cabal run --enable-coverage t3-feedback
./runhpc.sh t3-feedback
-}
-- | Definir aqui os testes do grupo para a Tarefa 3
testesTarefa3 :: [Estado]
testesTarefa3 = [e1,e2,e3,e4,e5]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3




e1 = Estado mapaTesteValido1 [] minhocasTeste1
e2 = Estado mapaTesteValido2 [] minhocasTeste1
e3 = Estado mapaTesteValido3 [] minhocasTeste1
e4 = Estado mapaTesteValido4 [] minhocasTeste2
e5 = Estado mapaTesteValido1 objetosTeste1 []




















-- Mapas válido
mapaTesteValido1 =
  [ [Ar, Ar, Terra, Pedra]
  , [Ar, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido2 =
  [ [Agua, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Agua, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido3 =
  [ [Agua, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido4 =
  [ [Agua, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Ar, Pedra, Terra, Agua]
  ]



minhocasTeste1 =
  [ Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste2 =
  [ Minhoca (Just (3,0)) (Viva 100) 1 1 1 1 1 ]





objetosTeste1 =
  [ Barril (2,2) True ]
 