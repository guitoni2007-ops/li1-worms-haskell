{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.
-}
module Tarefa4 where

import Data.Either

import Labs2025
import Tarefa2
import Tarefa3

-- | Função principal da Tarefa 4. Dado um estado retorna uma lista de jogadas, com exatamente 100 jogadas.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Aplica uma sequência de jogadas a um estado, avançando o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada de uma minhoca a um estado, e avança o tempo.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

-- | Avança o tempo para o estado de uma minhoca, se não efetuou a última jogada.
avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

-- | Avança o tempo para o estado de um objeto, se não foi criado pela última jogada.
avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- | Para um número de ticks desde o início da tática, dado um estado, determina a próxima jogada.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica t e =
    let minhocas = minhocasEstado e
        -- Seleciona a minhoca que vai jogar (ciclo pelas minhocas disponíveis)
        minhocaAtual = minhocas !! (t `mod` length minhocas)
        Just (x, y) = posicaoMinhoca minhocaAtual
        -- Verifica se há minhocas adversárias no alcance
        minhocasAdversarias = filter (\m -> posicaoMinhoca m /= posicaoMinhoca minhocaAtual) minhocas
        -- Verifica se há barris no alcance
        barris = filter (\(Barril pos _) -> pos == (x + 1, y) || pos == (x - 1, y) || pos == (x, y + 1) || pos == (x, y - 1)) (objetosEstado e)
    in
        if not (null minhocasAdversarias) then
            -- Se houver minhocas adversárias, ataca a mais próxima
            let Just (ax, ay) = posicaoMinhoca (head minhocasAdversarias)
            in if ax > x then (0, Move Sul) -- Move para baixo
               else if ax < x then (0, Move Norte) -- Move para cima
               else if ay > y then (0, Move Este) -- Move para a direita
               else if ay < y then (0, Move Oeste) -- Move para a esquerda
               else (0, Dispara Bazuca Norte) -- Ataca com a bazuca
        else if not (null barris) then
            -- Se não houver minhocas, mas houver barris, ataca o barril mais próximo
            let (bx, by) = posicaoBarril (head barris)
            in if bx > x then (0, Move Sul)
               else if bx < x then (0, Move Norte)
               else if by > y then (0, Move Este)
               else if by < y then (0, Move Oeste)
               else (0, Dispara Bazuca Norte)
        else
            -- Caso contrário, move-se para explorar o mapa
            if t `mod` 4 == 0 then (0, Move Norte)
            else if t `mod` 4 == 1 then (0, Move Este)
            else if t `mod` 4 == 2 then (0, Move Sul)
            else (0, Move Oeste)
