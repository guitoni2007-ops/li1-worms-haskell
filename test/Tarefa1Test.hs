module Main where

import Labs2025
import Tarefa1
import Magic

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = [e1]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1


e1 = Estado mapaTesteValido objetosTeste minhocasTeste

mapaTesteValido =
    [ [Ar, Ar, Terra, Pedra]
    , [Agua, Terra, Terra, Pedra]
    , [Ar, Ar, Ar, Ar]
    , [Pedra, Pedra, Terra, Agua]]

mapaTesteInvalido1 =
    [ [Ar, Ar, Terra, Pedra]
    , [Agua, Terra, Terra, Pedra]
    , [Ar, Ar, Lava, Ar]
    , [Pedra, Pedra, Terra, Agua]]

mapaTesteInvalido2 =
    [ [Ar, Ar, Terra, Pedra]
    , [Agua, Terra, Terra, Pedra]
    , [Ar, Ar, Ar, Ar, Ar]
    , [Pedra, Pedra, Terra, Agua]]

mapaTesteInvalido3 = [[]]


objetosTeste =
  [ Barril
      { posicaoBarril = (1,1)
      , explodeBarril = False
      }
  , Barril
      { posicaoBarril = (2,2)
      , explodeBarril = True
      }
  , Disparo
      { posicaoDisparo = (3,3)
      , direcaoDisparo = Norte
      , tipoDisparo = Dinamite
      , tempoDisparo = Just 3
      , donoDisparo = 2
      }
  , Disparo
      { posicaoDisparo = (1,3)
      , direcaoDisparo = Sul
      , tipoDisparo = Bazuca
      , tempoDisparo = Nothing
      , donoDisparo = 1
      }
  , Disparo
      { posicaoDisparo = (3,2)
      , direcaoDisparo = Este
      , tipoDisparo = Mina
      , tempoDisparo = Just 1
      , donoDisparo = 0
      }
  ]


minhocasTeste =
  [ Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (3,3)) (Viva 100) 1 1 1 1 1
  ]