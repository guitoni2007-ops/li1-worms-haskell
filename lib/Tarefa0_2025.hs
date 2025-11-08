{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
encontraQuantidadeArmaMinhoca arma minhoca =
                    case arma of
                        Jetpack -> jetpackMinhoca minhoca
                        Escavadora -> escavadoraMinhoca minhoca
                        Bazuca -> bazucaMinhoca minhoca
                        Mina -> minaMinhoca minhoca
                        Dinamite -> dinamiteMinhoca minhoca
-- | Atualiza a quantidade de munições disponíveis de uma minhoca para uma dada arma.
atualizaQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int -> Minhoca
atualizaQuantidadeArmaMinhoca arma minhoca novaQuantidade =
                 case arma of
                    Jetpack -> minhoca { jetpackMinhoca    = novaQuantidade }
                    Escavadora -> minhoca { escavadoraMinhoca = novaQuantidade }
                    Bazuca     -> minhoca { bazucaMinhoca     = novaQuantidade }
                    Mina       -> minhoca { minaMinhoca       = novaQuantidade }
                    Dinamite   -> minhoca { dinamiteMinhoca   = novaQuantidade }
-- | Incrementa ou decrementa a quantidade de munições de uma arma numa minhoca.
alteraQuantidadeArmaMinhoca :: TipoArma -> Int -> Minhoca -> Minhoca
alteraQuantidadeArmaMinhoca arma delta minhoca =
    let quantidadeAtual = encontraQuantidadeArmaMinhoca arma minhoca
        novaQuantidade  = quantidadeAtual + delta
    in atualizaQuantidadeArmaMinhoca arma minhoca novaQuantidade
-- | Dispara uma arma, reduzindo automaticamente a munição em 1.
disparaArma :: TipoArma -> Minhoca -> Minhoca
disparaArma arma minhoca =
    let qtdAtual = encontraQuantidadeArmaMinhoca arma minhoca
        novaQtd = max 0 (qtdAtual - 1)  -- garante que não fica negativa
    in atualizaQuantidadeArmaMinhoca arma minhoca novaQtd

-- | Verifica se um tipo de terreno é destrutível, i.e., pode ser destruído por explosões.
--
-- __NB:__ Apenas @Terra@ é destrutível.
eTerrenoDestrutivel :: Terreno -> Bool
eTerrenoDestrutivel Terra = True
eTerrenoDestrutivel _     = False   

-- | Verifica se um tipo de terreno é opaco, i.e., não permite que objetos ou minhocas se encontrem por cima dele.
--
-- __NB:__ Apenas @Terra@ ou @Pedra@ são opacos.
eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco Terra = True
eTerrenoOpaco Pedra = True
eTerrenoOpaco _     = False

-- | Verifica se uma posição do mapa está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se não contiver um terreno opaco.
ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre (x, y) mapa =
    let terreno = mapa !! x !! y  -- acede à posição (x,y) na matriz
    in not (eTerrenoOpaco terreno)

-- | Verifica se uma posição do estado está livre, i.e., pode ser ocupada por um objeto ou minhoca.
--
-- __NB:__ Uma posição está livre se o mapa estiver livre e se não estiver já uma minhoca ou um barril nessa posição.
ePosicaoEstadoLivre :: Posicao -> Estado -> Bool
ePosicaoEstadoLivre pos estado =
    let mapa = mapaEstado estado
    in if not (ePosicaoMatrizValida pos mapa)
       then False  -- fora do mapa → não está livre
       else
         let mapaLivre = ePosicaoMapaLivre pos mapa
             minhocaLivre = all (\m -> posicaoMinhoca m /= Just pos) (minhocasEstado estado)
             barrilLivre  = all (\o -> case o of
                                         Barril {posicaoBarril = p} -> p /= pos
                                         _ -> True) (objetosEstado estado)
         in mapaLivre && minhocaLivre && barrilLivre

-- | Verifica se numa lista de objetos já existe um disparo feito para uma dada arma por uma dada minhoca.
minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool
minhocaTemDisparo arma numMinhoca objetos =
    any (\o -> case o of
                 Disparo { tipoDisparo = t, donoDisparo = d } -> t == arma && d == numMinhoca
                 _ -> False)
        objetos

-- | Destrói uma dada posição no mapa (tipicamente como consequência de uma explosão).
--
-- __NB__: Só terrenos @Terra@ pode ser destruídos.
destroiPosicao :: Posicao -> Mapa -> Mapa
destroiPosicao (x, y) mapa =
    [ if yi == y
        then [ if xi == x && t == Terra then Ar else t | (xi, t) <- zip [0..] linha ]
        else linha
    | (yi, linha) <- zip [0..] mapa ]

-- Adiciona um novo objeto a um estado.
--
-- __NB__: A posição onde é inserido não é relevante.
adicionaObjeto :: Objeto -> Estado -> Estado
adicionaObjeto obj (Estado mapa objs minhocas) =
    Estado mapa (obj : objs) minhocas



