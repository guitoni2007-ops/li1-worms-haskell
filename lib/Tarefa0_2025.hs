{-|
Module      : Tarefa0_2025
Description : Funções auxiliares.

Módulo que define funções auxiliares que serão úteis na resolução do trabalho prático de LI1\/LP1 em 2025\/26.
-}

-- | Este módulo 
module Tarefa0_2025 where
    
import Labs2025

-- | Retorna a quantidade de munições disponíveis de uma minhoca para uma dada arma.
encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca = undefined

-- | Atualia a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca = undefined

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel = undefined

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco = undefined

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre = undefined

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre = undefined

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo = undefined

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao = undefined

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto = undefined



