{-|
Module      : Tarefa3
Description : Avançar tempo do jogo.

Módulo para a realização da Tarefa 3 de LI1\/LP1 em 2025\/26.
-}

module Tarefa3 where

import Data.Either

import Labs2025

import Tarefa0_2025

type Dano = Int
type Danos = [(Posicao,Dano)]


mapaTesteValido1 =
  [ [Ar, Ar, Terra, Pedra]
  , [Agua, Terra, Terra, Pedra]
  , [Ar, Ar, Ar, Ar]
  , [Pedra, Pedra, Terra, Agua]
  ]

minhocasTeste12 =
  Minhoca (Just (1,0)) (Viva 100) 1 1 1 1 1

minhocasTeste14 =
  [ Minhoca (Just (0,0)) (Viva 90) 1 1 1 1 1
  , Minhoca (Just (1,0)) (Viva 80) 1 1 1 1 1
  , Minhoca (Just (3,3)) (Viva 80) 1 1 1 1 1
  ]



















-- | Função principal da Tarefa 3. Avanço o estado do jogo um tick.
avancaEstado :: Estado -> Estado
avancaEstado e@(Estado mapa objetos minhocas) = foldr aplicaDanos e' danoss
    where
    minhocas' = map (uncurry $ avancaMinhoca e) (zip [0..] minhocas)
    (objetos',danoss) = partitionEithers $ map (uncurry $ avancaObjeto $ e { minhocasEstado = minhocas' }) (zip [0..] objetos)
    e' = Estado mapa objetos' minhocas'

-- | Para um dado estado, dado o índice de uma minhoca na lista de minhocas e o estado dessa minhoca, retorna o novo estado da minhoca no próximo tick.
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca e i m = undefined

-- | Para um dado estado, dado o índice de um objeto na lista de objetos e o estado desse objeto, retorna o novo estado do objeto no próximo tick ou, caso o objeto expluda, uma lista de posições afetadas com o dano associado.
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos
avancaObjeto e i o = undefined

-- | Para uma lista de posições afetadas por uma explosão, recebe um estado e calcula o novo estado em que esses danos são aplicados.
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos ds e = undefined

{-
--Verifica se uma lista de minhocas está sujeita ao efeito da gravidade
verificaGravidadeMinhocas :: [Minhoca] -> Bool
-}

--Verifica se o Terreno é não opaco
eTerrenonaoOpaco :: Terreno -> Bool
eTerrenonaoOpaco = not . eTerrenoOpaco


-- Verifica se uma minhoca tem um terreno válido para ser afetada pela gravidade
eMinhocaGravidade :: Mapa -> Minhoca -> Bool
eMinhocaGravidade mapa minhoca =
  case posicaoMinhoca minhoca of
    Just pos ->
      ePosicaoMatrizValida pos mapa &&
      ePosicaoMapaLivre pos mapa
    Nothing -> False


-- Verifica se uma lista de minhocas tem um terreno válido para ser afetada pela gravidade
eMinhocasGravidade :: Mapa -> [Minhoca] -> Bool 
eMinhocasGravidade mapa [] = True
eMinhocasGravidade mapa (m:ms) = eMinhocaGravidade mapa m && eMinhocasGravidade mapa ms




