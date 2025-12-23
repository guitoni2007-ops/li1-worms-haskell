module Main where

import Labs2025
import Tarefa4
import Magic
---cabal clean && rm -rf t4-feedback.tix
-- | Definir aqui os testes do grupo para a Tarefa 4
testesTarefa4 :: [Estado]
testesTarefa4 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19]

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4


e1 = Estado mapaTesteValido1 [] minhocasTeste1
e2 = Estado mapaTesteValido2 [] minhocasTeste1 
e3 = Estado mapaTesteValido1 [] minhocasTeste1
e4 = Estado mapaTesteValido2 [] minhocasTeste2
e5 = Estado mapaTesteValido1 objetosTeste1 minhocasTeste1
e6 = Estado mapaTesteValido1 objetosTeste2 minhocasTeste2
e7 = Estado mapaTesteValido1 objetosTeste3 minhocasTeste3
e8 = Estado mapaTesteValido2 objetosTeste4 minhocasTeste1
e9 = Estado mapaTesteValido1 objetosTeste5 minhocasTeste2
e10 = Estado mapaTesteValido1 objetosTeste6 minhocasTeste2
e11 = Estado mapaTesteValido1 objetosTeste7 minhocasTeste3
e12 = Estado mapaTesteValido2 objetosTeste1 minhocasTeste1
e13 = Estado mapaTesteValido1 objetosTeste7 minhocasTeste3

e14 = Estado mapaTesteValido1 [] minhocasDuelo
e15 = Estado mapaTesteValido3 [] minhocasDuelo2

e16 = Estado mapaTesteValido3 objetosTeste11 minhocasDuelo2

e17 = Estado mapaTesteValido2 [] minhocasTeste4

e18 = Estado mapaTesteValido4 [] minhocasTeste3

e19 = Estado mapaTesteValido3 objetosTeste11 minhocasDuelo3
-- Mapas válido
mapaTesteValido1 =
  [ [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
  , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua]
  ]

mapaTesteValido2 =
  [ [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Pedra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Agua,Agua,Agua]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
  , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua]
  ]

mapaTesteValido3 =
  [ [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Terra, Ar,Ar, Ar,Ar, Ar]
  , [Pedra,Terra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Agua,Agua,Agua]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
  , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua]
  ]

mapaTesteValido4 =
  [ [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar,Ar, Ar]
  , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Terra,Agua,Agua,Agua]
  , [Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Terra,Agua,Agua,Agua]
  , [Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Agua,Agua,Agua]
  ]
minhocasTeste1 =
  [ Minhoca (Just (2,8)) (Viva 100) 1 15 5 5 5 ]

minhocasTeste2 =
  [ Minhoca (Just (2,2)) (Viva 100) 1 15 5 5 5 ]

minhocasTeste3 =
  [ Minhoca (Just (2,4)) (Viva 100) 1 14 5 5 5 ]

minhocasTeste4 =
  [ Minhoca (Just (2,4)) (Viva 100) 1 0 0 0 0 ]
objetosTeste1 =
  [ Barril (2,10) False ]
 
objetosTeste2 =
  [ Barril (2,10) False ]
 
objetosTeste3 =
  [ Barril (2,10) False ]

objetosTeste11 =
  [ Barril (2,4) False ]
objetosTeste4 =
  [ Disparo (2,6) Norte Mina Nothing 5 ]

objetosTeste5 =
  [ Disparo (2,2) Norte Dinamite Nothing 0
  , Disparo (2,3) Nordeste Dinamite Nothing 0
  , Disparo (2,4) Sudoeste Dinamite Nothing 0
  , Disparo (2,3) Noroeste Dinamite Nothing 0
  , Disparo (2,1) Sudeste Dinamite Nothing 0
  , Disparo (2,9) Sul Dinamite Nothing 0
  ]

objetosTeste6 =
  [ Disparo (2,3) Sul Mina Nothing 5 ]

objetosTeste7 =
  [ Disparo (2,2) Sul Mina Nothing 5 ]

minhocasDuelo =
  [ Minhoca (Just (2,2)) (Viva 100) 1 15 5 5 5  -- Minhoca 0
  , Minhoca (Just (2,5)) (Viva 100) 1 15 5 5 5  -- Minhoca 1
  ]

minhocasDuelo2 =
  [ Minhoca (Just (2,2)) (Viva 10) 1 15 5 5 5  -- Minhoca 0
  , Minhoca (Just (2,5)) (Viva 100) 1 15 5 5 5  -- Minhoca 1
  , Minhoca (Just (1,8)) (Viva 30) 1 15 5 5 5
  ]

minhocasDuelo3 =
  [ Minhoca (Just (2,2)) (Viva 100) 1 15 5 5 5  -- Minhoca 0
  , Minhoca (Just (1,8)) (Viva 100) 1 15 5 5 5  -- Minhoca 1
  ]