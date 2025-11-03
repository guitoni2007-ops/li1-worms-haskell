{-|
Module      : Tarefa1
Description : Validação de estados.

Módulo para a realização da Tarefa 1 de LI1\/LP1 em 2025\/26.
-}
module Tarefa1 where

import Labs2025 
import Tarefa0_2025




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
  [ Minhoca (Just (2,1)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1
  , Minhoca (Just (3,0)) (Viva 100) 1 1 1 1 1
  ]




{-

-- | Função principal da Tarefa 1. Recebe um estado e retorna se este é válido ou não.
validaEstado :: Estado -> Bool
validaEstado estado =
  let mapa     = mapaEstado estado
      objetos  = objetosEstado estado
      minhocas = minhocasEstado estado
  in validaMapa mapa && validaObj objetos minhocas && validaWorms minhocas
-}


-- | 1 Função secundária da Tarefa 1. Recebe um mapa e retorna se este é válido ou não.
validaMapa :: Mapa -> Bool
validaMapa mapa = eMatrizValida(mapa) && eTerrenosValido(mapa)


-- Aux a eTerrenoValido.
eTerrenosValido :: Mapa -> Bool
eTerrenosValido [] = True
eTerrenosValido (t1:ts) = eTerrenoValido t1 && eTerrenosValido ts

-- Valida se o terreno é algum dos 4 possíveis.
eTerrenoValido :: [Terreno] -> Bool
eTerrenoValido [] = True
eTerrenoValido (t1:ts) = if (t1 == Ar || t1 == Agua || t1 == Terra || t1 == Pedra) then eTerrenoValido(ts)
                         else False




-- | 2 Função secundária da Tarefa 1. Verifica se uma lista de objetos é válida ou não.
validaObj :: Posicao -> Mapa -> [Objeto] -> [Minhoca] -> Bool
validaObj posicao mapa objetos minhocas = posicaovalidalivre posicao mapa && objetosPosicao objetos minhocas && objeto_disparo objetos minhocas

-- Verifica se uma posição é válida e livre no mapa.
posicaovalidalivre :: Posicao -> Mapa -> Bool
posicaovalidalivre (l,c) mapa = ePosicaoMatrizValida (l,c) mapa && not (eTerrenoOpaco (mapa !! l !! c))

--Verifica se duas posições sao diferentes.
posicoesDiferentes :: Posicao -> Posicao -> Bool
posicoesDiferentes p1 p2 = p1 /= p2

--Verifica se duas posições sao diferentes para maybe posição.
posicoesDiferentes2 :: Posicao -> Maybe Posicao -> Bool
posicoesDiferentes2 p1 (Just p2) = p1 /= p2
posicoesDiferentes2 _ Nothing    = True


--Verificar a posição do barril em relação a todas as minhocas.
verificaPosBarrilMinhoca :: Posicao -> [Minhoca] -> Bool
verificaPosBarrilMinhoca pos1 [] = True
verificaPosBarrilMinhoca pos1 ((Minhoca pos vida jet escava bazuca mina dina):ms) = posicoesDiferentes2 pos1 pos && verificaPosBarrilMinhoca pos1 ms

--Verifica se as posições do barris e das minhocas não são iguais.
verificaPosBarril :: Objeto -> [Objeto] -> [Minhoca] -> Bool
verificaPosBarril (Barril pos1 explode1) [] m = verificaPosBarrilMinhoca pos1 m
verificaPosBarril (Barril pos1 explode1) ((Barril pos2 explode2):os) minhocas = posicoesDiferentes pos1 pos2 && verificaPosBarrilMinhoca pos1 minhocas && verificaPosBarril (Barril pos1 explode1) os minhocas

--Verifica se as posições da mina e da dinamite
verificaPosMinaDina :: Objeto -> [Objeto] -> Bool
verificaPosMinaDina o [] = True
verificaPosMinaDina (Disparo pos1 dir1 tipo1 tempo1 dono1) ((Barril pos2 explode2):os) = posicoesDiferentes pos1 pos2 && verificaPosMinaDina (Disparo pos1 dir1 tipo1 tempo1 dono1) os

-- Filtra uma lista de objetos retornando uma lista só de barris.
filtraBarris :: [Objeto] -> [Objeto]
filtraBarris = filter isBarril
  where
    isBarril (Barril _ _) = True
    isBarril _            = False


--Verifica a posição de objetos especificos (Barril, Dinamite ou Mina).
objetoPosicao :: Objeto -> [Objeto] -> [Minhoca] -> Bool
objetoPosicao objeto listaObjetos minhocas =
                                    case objeto of
                                        Barril pos explode -> verificaPosBarril objeto (filtraBarris listaObjetos) minhocas

                                        Disparo pos dir tipo tempo dono ->
                                            case tipo of
                                                Dinamite -> verificaPosMinaDina objeto (filtraBarris listaObjetos)
                                                Mina     -> verificaPosMinaDina objeto (filtraBarris listaObjetos)
                                                _        -> True



--AuxobjetosPosicao. 
proximoObjeto :: [Objeto] -> Maybe (Objeto, [Objeto])
proximoObjeto []     = Nothing
proximoObjeto (o:os) = Just (o, os)


removerObjeto :: Objeto -> [Objeto] -> [Objeto]
removerObjeto obj = filter (/= obj)


--Recebe uma lista de objetos e passa a "objetoPosicao" um objeto de cada vez.
objetosPosicao :: [Objeto] -> [Minhoca] -> Bool
objetosPosicao todos minhocas = verificar todos
  where
    verificar [] = True
    verificar lista =
      case proximoObjeto lista of
        Nothing -> True
        Just (obj, _) ->
          objetoPosicao obj (removerObjeto obj todos) minhocas && verificar (tail lista)


 


-- Dado uma lista de disparos, verifica se algum elemento é do tipo de arma: Jetpack ou Escavadora.
dispJetEsc :: [Objeto] -> Bool
dispJetEsc [] = True
dispJetEsc ((Disparo _ _ tipo _ _):os) =
  tipo /= Jetpack && tipo /= Escavadora && dispJetEsc os
dispJetEsc (_:os) = dispJetEsc os




-- Dado uma lista de disparos, verifica se o tempo do disparo é válido para o tipo de arma.
validatempoDisparo :: [Objeto] -> Bool
validatempoDisparo [] = True
validatempoDisparo ((Disparo _ _ tipo tempo _):os) =
  case tempo of
    Just t ->
      case tipo of
        Mina     -> (t >= 0 && t <= 2) && validatempoDisparo os
        Dinamite -> (t >= 0 && t <= 4) && validatempoDisparo os
        _        -> validatempoDisparo os
    Nothing ->   
      case tipo of
        Bazuca -> validatempoDisparo os
        Mina -> validatempoDisparo os
        _ -> validatempoDisparo os
validatempoDisparo (_:os) = validatempoDisparo os

-- Verifica se o dono do disparo tem um índice válido na lista de minhocas.
validaDonoDisparo :: [Objeto] -> [Minhoca] -> Bool
validaDonoDisparo [] _ = True
validaDonoDisparo ((Disparo _ _ _ _ dono):os) minhocas =
  eIndiceListaValido dono minhocas && validaDonoDisparo os minhocas
validaDonoDisparo (_:os) minhocas = validaDonoDisparo os minhocas

-- Verifica se o mesmo dono nao tem simultaneamente mais do que um disparo de cada tipo.
maisQueUmDisparo :: [Objeto] -> Bool
maisQueUmDisparo objetos = verifica [] objetos
 
 -- Função auxiiar de maisQueUmDisparo.
verifica :: [(NumMinhoca, TipoArma)] -> [Objeto] -> Bool
verifica _ [] = True
verifica vistos (Disparo _ _ tipo _ dono : os)
    | (dono, tipo) `elem` vistos = False
    | otherwise = verifica ((dono, tipo) : vistos) os
verifica vistos (_ : os) = verifica vistos os



-- Recebe uma lista de objetos e verifica se os disparos são válidos.
objeto_disparo :: [Objeto] -> [Minhoca] -> Bool
objeto_disparo objetos minhocas = dispJetEsc objetos && validatempoDisparo objetos && validaDonoDisparo objetos minhocas && maisQueUmDisparo objetos

-- | 3 Função secundária da Tarefa 1. Recebe uma lista de minhocas e retorna se este é válido ou não.
{-
validaWorms : [Minhocas] -> Bool
-}







                                                        
                                                      
                                                          
                                                 



 




 








































{-
-- | 3 Função secundária da Tarefa 1. Recebe uma lista de minhocas e retorna se este é válida ou não.
validaWorms :: [Minhoca] -> Bool

  in validaMapa mapa && validaObj objetos && validaWorms minhocas
-}
