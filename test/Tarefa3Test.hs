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
testesTarefa3 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20]

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
e12 = Estado mapaTesteValido3 objetosTeste11 minhocasTeste3
e13 = Estado mapaTesteValido5 objetosTeste7 minhocasTeste3
e14 = Estado mapaTesteValido4 objetosTeste9 minhocasTeste3
e15 = Estado mapaTesteValido4 objetosTeste10 minhocasTeste3
e16 = Estado mapaTesteValido4 objetosTeste10 minhocasTeste4
e17 = Estado mapaTesteValido4 objetosTeste12 minhocasTeste3
e18 = Estado mapaTesteValido5 objetosTeste13 minhocasTeste5
e19 = Estado mapaTesteValido7 objetosTeste2 []
e20 = Estado mapaTesteValido7 objetosTeste14 []      











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
  , [Ar, Ar, Terra, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido4 =
  [ [Agua, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Ar, Pedra, Terra, Terra]
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

mapaTesteValido7 =
  [ [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Agua, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar]
  ]

minhocasTeste1 =
  [ Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste2 =
  [ Minhoca (Just (3,0)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste3 =
  [ Minhoca (Just (2,3)) (Viva 100) 1 1 1 1 1 ]

minhocasTeste4 =
  [ Minhoca (Just (2,3)) Morta 1 1 1 1 1 ]

minhocasTeste5 =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1 ]


objetosTeste1 =
  [ Barril (2,2) True ]
 
objetosTeste2 =
  [ Barril (2,2) False ]
 
objetosTeste3 =
  [ Disparo (3,3) Norte Bazuca Nothing 5 ]

objetosTeste4 =
  [ Disparo (0,0) Norte Bazuca Nothing 5 ]

objetosTeste5 =
  [ Disparo (1,2) Norte Dinamite Nothing 1
  , Disparo (2,3) Nordeste Dinamite Nothing 2
  , Disparo (2,4) Sudoeste Dinamite Nothing 3
  , Disparo (3,3) Noroeste Dinamite Nothing 4
  , Disparo (1,1) Sudeste Dinamite Nothing 5
  , Disparo (3,2) Sul Dinamite Nothing 6
  , Disparo (1,0) Este Dinamite Nothing 7
  , Disparo (2,2) Oeste Dinamite Nothing 8
  ]


objetosTeste6 =
  [ Disparo (3,3) Sul Mina Nothing 5 ]

objetosTeste7 =
  [ Disparo (2,2) Sul Mina Nothing 5 ]

objetosTeste8 =
  [ Disparo (3,3) Norte Escavadora (Just 2) 5 ]

objetosTeste9 =
  [ Disparo (2,2) Sul Dinamite (Just 0) 5 ]

objetosTeste10 =
  [ Disparo (2,2) Sul Dinamite (Just 3) 5 ]

objetosTeste11 =
  [ Disparo (1,2) Sul Bazuca (Just 0) 5 ]

objetosTeste12 =
  [ Disparo (1,2) Sul Dinamite (Just 0) 5 ]

objetosTeste13 =
  [ Disparo (2,2) Sul Dinamite (Just 0) 5 ]

objetosTeste14 =
  [ Disparo (2,2) Sul Mina (Just 1) 5 ]