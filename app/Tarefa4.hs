{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.

Módulo para a realização da Tarefa 4 de LI1\/LP1 em 2025\/26.
-}
module Tarefa4 where

import Data.Either
import Data.Maybe (fromMaybe)
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
jogadaTatica t e = (idx, acao)
  where
    minhocas = minhocasEstado e
    -- 1. Seleciona a minhoca viva (ciclo)
    vivas = [i | (i, m) <- zip [0..] minhocas, vidaMinhoca m /= Morta]
    idx = if null vivas then 0 else vivas !! (t `mod` length vivas)
    mAtual = minhocas !! idx
    
    -- 2. Define a ação baseada na posição atual
    acao = case posicaoMinhoca mAtual of
        Nothing -> Move Norte
        Just (x, y) -> decidir (x, y)
    
    decidir (x, y)
        -- 1. Atacar: Só se houver inimigo VIVO alinhado
        | not (null alvosVivos) = Dispara Bazuca (dirPara (head alvosVivos) (x, y))
        
        -- 2. Escavar: Se houver Terra adjacente
        | not (null terras) = Dispara Escavadora (head terras)
        
        -- 3. Perseguir: Se houver inimigos vivos noutro lado
        | not (null inimigosVivos) = Move (dirPara (posicaoInimigo (head inimigosVivos)) (x, y))
        
        -- 4. EXPLORAR: Caso não haja nada acima, volta ao movimento aleatório
        | otherwise = [Move Norte, Move Este, Move Sul, Move Oeste] !! (t `mod` 4)
      where
        -- Filtra inimigos que estão na mesma linha/coluna e que estão VIVOS
        alvosVivos = [p | m <- inimigosVivos, 
                         let p = posicaoInimigo m, 
                         vidaMinhoca m /= Morta,
                         let (ax, ay) = p, ax == x || ay == y]

        terras = [d | d <- [Norte, Este, Sul, Oeste], 
                      let pDest = movePosicao d (x, y),
                      case encontraPosicaoMatriz pDest (mapaEstado e) of
                          Just Terra -> True
                          _ -> False]

    -- Funções auxiliares globais ao jogadaTatica
    inimigosVivos = [m | (i, m) <- zip [0..] minhocas, i /= idx, vidaMinhoca m /= Morta]
    
    posicaoInimigo m = fromMaybe (0,0) (posicaoMinhoca m)

    dirPara (ax, ay) (cx, cy)
        | ax < cx = Norte
        | ax > cx = Sul
        | ay > cy = Este
        | ay < cy = Oeste
        | otherwise = Norte