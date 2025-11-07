module Main where

import Labs2025
import Tarefa1
import Magic
{-
cabal clean && rm -rf t1-feedback.tix
cabal run --enable-coverage t1-feedback
./runhpc.sh t1-feedback
-}

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22,e23,e24,e25,e26,e27,e28]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1


e1 = Estado mapaTesteValido1 objetosTeste1 minhocasTeste1
e2 = Estado mapaTesteInvalido1 objetosTeste1 minhocasTeste1
e3 = Estado mapaTesteValido2 objetosTeste2 minhocasTeste2  
e4 = Estado mapaTesteValido3 objetosTeste2 minhocasTeste2
e5 = Estado mapaTesteValido3 objetosTeste1 minhocasTeste1
e6 = Estado mapaTesteValido5 objetosTeste3 minhocasTeste3
e7 = Estado mapaTesteValido6 objetosTeste4 minhocasTeste4
e8 = Estado mapaTesteValido7 objetosTeste5 minhocasTeste5
e9 = Estado mapaTesteValido8 objetosTeste6 minhocasTeste5
e10 = Estado mapaTesteInvalido3 objetosTeste6 minhocasTeste5
e11 = Estado mapaTesteInvalido2 objetosTeste6 minhocasTeste5
e12 = Estado mapaTesteInvalido4 objetosTeste6 minhocasTeste5
e13 = Estado mapaTesteValido1 objetosTeste6 minhocasTeste5
e14 = Estado mapaTesteValido1 objetosTeste5 minhocasTeste5
e15 = Estado mapaTesteValido1 objetosTeste4 minhocasTeste2
e16 = Estado mapaTesteValido1 objetosTeste5 minhocasTeste2 
e17 = Estado mapaTesteValido3 objetosTeste2 minhocasTeste2
e18 = Estado mapaTesteValido7 objetosTeste7 minhocasTeste1
e19 = Estado mapaTesteValido7 objetosTeste8 minhocasTeste1
e20 = Estado mapaTesteValido5 objetosTeste9 minhocasTeste3
e21 = Estado mapaTesteValido5 objetosTeste3 minhocasTeste6
e22 = Estado mapaTesteValido7 objetosTeste1 minhocasTeste6
e23 = Estado mapaTesteValido8 objetosTeste1 minhocasTeste7
e24 = Estado mapaTesteValido8 objetosTeste1 minhocasTeste8
e25 = Estado mapaTesteValido8 objetosTeste1 minhocasTeste9
e26 = Estado mapaTesteValido8 objetosTeste1 minhocasTeste10
e27 = Estado mapaTesteValido9 objetosTeste10 minhocasTeste12
e28 = Estado mapaTesteInvalido3 objetosTeste11 minhocasTeste11
-- Mapas válido
mapaTesteValido1 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido2 =
  [ [Terra, Ar, Ar, Pedra]
  , [Agua, Terra, Pedra, Pedra]
  , [Ar, Ar, Terra, Ar]
  , [Pedra, Terra, Terra, Agua]
  ]

mapaTesteValido3 =
  [ [Ar, Terra, Terra, Pedra]
  , [Agua, Pedra, Terra, Pedra]
  , [Ar, Ar, Ar, Terra]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido4 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Pedra, Pedra]
  , [Ar, Terra, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteValido5 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Ar, Ar, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Ar, Agua]
  ]

mapaTesteValido6 =
  [ [Ar, Ar, Ar, Ar]
  , [Ar, Terra, Ar, Ar]
  , [Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Agua]
  ]
mapaTesteValido7 =
  [ [Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Terra, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Agua, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  ]
mapaTesteValido8 =
  [ [Ar, Terra, Ar, Pedra, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Agua, Ar]
  , [Ar, Terra, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  , [Ar, Ar, Ar, Ar, Ar, Ar]
  ]

mapaTesteValido9 =
  [ [Pedra, Pedra, Ar], [Ar, Ar, Ar], [Ar, Ar, Ar]]





-- Mapas inválidos
mapaTesteInvalido1 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Lava, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteInvalido2 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

mapaTesteInvalido3 = [[]]

mapaTesteInvalido4 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Lava]
  , [Pedra, Pedra, Terra, Agua]
  ]


-- Objetos de teste
objetosTeste1 =
  [ Barril { posicaoBarril = (5,5), explodeBarril = False }
  , Barril { posicaoBarril = (2,2), explodeBarril = True }
  , Disparo { posicaoDisparo = (3,3), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 2 }
  , Disparo { posicaoDisparo = (1,3), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 1 }
  , Disparo { posicaoDisparo = (3,2), direcaoDisparo = Este, tipoDisparo = Mina, tempoDisparo = Just 1, donoDisparo = 0 }
  ]

objetosTeste2 =
  [ Barril { posicaoBarril = (0,2), explodeBarril = True }
  , Barril { posicaoBarril = (3,0), explodeBarril = False }
  , Disparo { posicaoDisparo = (2,1), direcaoDisparo = Oeste, tipoDisparo = Bazuca, tempoDisparo = Just 2, donoDisparo = 1 }
  , Disparo { posicaoDisparo = (0,3), direcaoDisparo = Norte, tipoDisparo = Jetpack, tempoDisparo = Nothing, donoDisparo = 0 }
  , Disparo { posicaoDisparo = (1,2), direcaoDisparo = Sul, tipoDisparo = Dinamite, tempoDisparo = Just 4, donoDisparo = 2 }
  ]

objetosTeste3 =
  [ Barril
      { posicaoBarril = (2,0)
      , explodeBarril = False
      }
  , Disparo
      { posicaoDisparo = (0,1)
      , direcaoDisparo = Sul
      , tipoDisparo = Bazuca
      , tempoDisparo = Nothing
      , donoDisparo = 0
      }
  , Disparo
      { posicaoDisparo = (2,2)
      , direcaoDisparo = Norte
      , tipoDisparo = Dinamite
      , tempoDisparo = Just 3
      , donoDisparo = 1
      }
  , Disparo
      { posicaoDisparo = (1,2)
      , direcaoDisparo = Este
      , tipoDisparo = Mina
      , tempoDisparo = Just 1
      , donoDisparo = 2
      }
  ]
objetosTeste4 =
  [ Barril
      { posicaoBarril = (0,1)
      , explodeBarril = False
      }
  , Disparo
      { posicaoDisparo = (0,2)
      , direcaoDisparo = Sul
      , tipoDisparo = Bazuca
      , tempoDisparo = Nothing
      , donoDisparo = 0
      }
  , Disparo
      { posicaoDisparo = (2,2)
      , direcaoDisparo = Norte
      , tipoDisparo = Dinamite
      , tempoDisparo = Just 2
      , donoDisparo = 1
      }
  , Disparo
      { posicaoDisparo = (1,3)
      , direcaoDisparo = Oeste
      , tipoDisparo = Mina
      , tempoDisparo = Just 1
      , donoDisparo = 2
      }
  ]
objetosTeste5 =
  [ Barril { posicaoBarril = (3,0), explodeBarril = False }
  , Barril { posicaoBarril = (4,4), explodeBarril = True }
  , Disparo { posicaoDisparo = (1,2), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0 }
  , Disparo { posicaoDisparo = (2,3), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 4, donoDisparo = 1 }
  , Disparo { posicaoDisparo = (3,0), direcaoDisparo = Este, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 2 }
  , Disparo { posicaoDisparo = (5,5), direcaoDisparo = Oeste, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 3 }
  , Disparo { posicaoDisparo = (4,1), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 1, donoDisparo = 4 }
  ]

objetosTeste6 =
  [ Barril { posicaoBarril = (0,5), explodeBarril = False }
  , Barril { posicaoBarril = (4,4), explodeBarril = True }

  , Disparo { posicaoDisparo = (1,2), direcaoDisparo = Sul, tipoDisparo = Bazuca, tempoDisparo = Nothing, donoDisparo = 0 }
  , Disparo { posicaoDisparo = (2,3), direcaoDisparo = Norte, tipoDisparo = Dinamite, tempoDisparo = Just 3, donoDisparo = 1 }
  , Disparo { posicaoDisparo = (3,0), direcaoDisparo = Este, tipoDisparo = Mina, tempoDisparo = Just 2, donoDisparo = 2 }
  ]

objetosTeste7 =
  [ Barril (1,1) False
  , Barril (2,2) True
  , Disparo (3,3) Norte Dinamite (Just 5) 2   
  , Disparo (1,3) Sul Bazuca (Just 2) 1       
  , Disparo (3,2) Este Mina (Just 3) 0      
  ]

objetosTeste8 =
  [ Disparo (0,1) Norte Bazuca Nothing 5 ]

objetosTeste9 =
  [ Disparo (0,1) Norte Bazuca Nothing 0
  , Disparo (1,1) Sul Bazuca Nothing 0
  ]


objetosTeste10 =
  [ Disparo (0,1) Este Bazuca Nothing 0 ]

objetosTeste11 = []










-- Minhocas de teste
minhocasTeste1 =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (3,3)) (Viva 100) 1 1 1 1 1
  ]

minhocasTeste2 =
  [ Minhoca (Just (0,1)) (Viva 90) 2 1 1 1 1
  , Minhoca (Just (2,2)) (Viva 80) 1 2 1 1 1
  , Minhoca (Just (3,0)) (Viva 70) 1 1 2 1 1
  ]
minhocasTeste3 =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,1)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (2,3)) (Viva 80) 1 1 1 1 1
  ]
minhocasTeste4 =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,1)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (2,0)) (Viva 80) 1 1 1 1 1
  ]
minhocasTeste5 =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,1)) (Viva 95) 1 1 1 1 1
  , Minhoca (Just (2,0)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (4,0)) (Viva 85) 1 1 1 1 1
  , Minhoca (Just (5,0)) (Viva 80) 1 1 1 1 1
  ]

minhocasTeste6 =
 [ Minhoca (Just (0,3)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,1)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (2,1)) (Viva 80) 1 1 1 1 1
  ]

minhocasTeste7 =
 [ Minhoca (Just (0,4)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,4)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (4,5)) (Viva 80) 1 1 1 1 1
  ]

minhocasTeste8 =
 [ Minhoca (Just (0,4)) (Morta) 1 1 1 1 1
  , Minhoca (Just (1,3)) (Morta) 1 1 1 1 1
  , Minhoca (Just (4,5)) (Viva 80) 1 1 1 1 1
  ]

minhocasTeste9 =
 [ Minhoca (Just (0,4)) (Morta) 1 1 1 1 1
  , Minhoca (Just (1,3)) (Morta) 1 1 1 1 1
  , Minhoca (Just (4,5)) (Viva 120) 1 1 1 1 1
  ]

minhocasTeste10 =
 [ Minhoca (Just (0,4)) (Morta) 1 1 1 1 1
  , Minhoca (Just (1,3)) (Morta) 1 1 1 1 1
  , Minhoca (Just (4,5)) (Viva 100) (-1) 1 1 1 1
  ]

minhocasTeste11 = []

minhocasTeste12 =
  [ Minhoca (Just (2,0)) (Viva 100) 1 1 1 1 1 ]



