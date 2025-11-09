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
testesTarefa3 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13]

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
e5 = Estado mapaTesteValido1 objetosTeste1 minhocasTeste1
e6 = Estado mapaTesteValido5 objetosTeste2 []
e7 = Estado mapaTesteValido5 objetosTeste3 []
e8 = Estado mapaTesteValido5 objetosTeste4 []
e9 = Estado mapaTesteValido5 objetosTeste5 []
e10 = Estado mapaTesteValido5 objetosTeste6 []
e11 = Estado mapaTesteValido6 objetosTeste7 []
e12 = Estado mapaTesteValido6 objetosTeste8 []
e13 = Estado mapaTesteValido5 objetosTeste7 minhocasTeste3















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

mapaTesteValido5 =
  [ [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  ]
mapaTesteValido6 =
  [ [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Terra, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  ]


minhocasTeste1 =
  [ Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste2 =
  [ Minhoca (Just (3,0)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste3 =
  [ Minhoca (Just (2,2)) (Viva 100) 1 1 1 1 1 ]





objetosTeste1 =
  [ Barril (2,2) True ]
 
objetosTeste2 =
  [ Barril (2,2) False ]
 
objetosTeste3 =
  [ Disparo (3,3) Norte Bazuca Nothing 5 ]

objetosTeste4 =
  [ Disparo (0,0) Norte Bazuca Nothing 5 ]

objetosTeste5 =
  [ Disparo (1,2) Norte Dinamite Nothing 0
  , Disparo (2,3) Nordeste Dinamite Nothing 0
  , Disparo (2,4) Sudoeste Dinamite Nothing 0
  , Disparo (3,3) Noroeste Dinamite Nothing 0
  , Disparo (1,1) Sudeste Dinamite Nothing 0
  , Disparo (3,2) Sul Dinamite Nothing 0
  ]

objetosTeste6 =
  [ Disparo (3,3) Sul Mina Nothing 5 ]

objetosTeste7 =
  [ Disparo (2,2) Sul Mina Nothing 5 ]

objetosTeste8 =
  [ Disparo (3,3) Norte Escavadora (Just 2) 5 ]