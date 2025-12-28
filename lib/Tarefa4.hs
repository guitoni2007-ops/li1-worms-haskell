{-|
Module      : Tarefa4
Description : Implementar uma tática de jogo.
-}
module Tarefa4 where

import Data.Either
import Data.Maybe (fromMaybe)
import Labs2025
import Tarefa2
import Tarefa3

-- | Função principal da Tarefa 4.
tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

-- | Avança o tempo entre jogadas.
avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

-- | Aplica uma jogada e os danos resultantes.
avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- | Determina a próxima jogada com foco em pontuação máxima e suicídio estratégico.
jogadaTatica :: Ticks -> Estado -> (NumMinhoca, Jogada)
jogadaTatica t e = (idx, acao)
  where
    minhocas = minhocasEstado e
    vivas = [i | (i, m) <- zip [0..] minhocas, vidaMinhoca m /= Morta]
    idx = if null vivas then 0 else vivas !! (t `mod` length vivas)
    mAtual = minhocas !! idx
    
    acao = case posicaoMinhoca mAtual of
        Nothing -> Move Norte
        Just (x, y) -> decidir (x, y)
    
    decidir (x, y)
        -- 1. Atacar inimigos alinhados
        | not (null alvosVivos) && bazucaMinhoca mAtual > 0 = 
            Dispara Bazuca (dirPara (head alvosVivos) (x, y))
        
        -- 2. Escavar terra adjacente (Prioridade de munição)
        | escavadoraMinhoca mAtual > 0 && not (null terrasAdj) = 
            Dispara Escavadora (head terrasAdj)
        
        -- 3. Bazuca na terra (Só se estiver alinhada para não falhar)
        | bazucaMinhoca mAtual > 0 && not (null terrasAlinhadas) =
            Dispara Bazuca (dirPara (head terrasAlinhadas) (x, y))
        
        -- 4. Ir para a Água (Suicídio = 100 pontos se tiver HP cheio)
        | not (null aguasNoMapa) = Move (dirPara (head aguasNoMapa) (x, y))

        -- 5. Ir para a Terra (Para aproximar e usar escavadora ou alinhar bazuca)
        | not (null todasTerras) = Move (dirPara (head todasTerras) (x, y))
        
        -- 6. Sair do mapa / Explorar Este (Evita ficar preso)
        | otherwise = Move Este
      where
        -- CORREÇÃO: Definir (ax, ay) ANTES de usar no filtro
        alvosVivos = [p | m <- inimigosVivos, 
                         let p = posicaoInimigo m, 
                         let (ax, ay) = p, 
                         ax == x || ay == y]
        
        terrasAdj = [d | d <- [Norte, Este, Sul, Oeste], 
                        let pDest = movePosicao d (x, y),
                        encontraPosicaoMatriz pDest (mapaEstado e) == Just Terra]

        todasTerras = [(lx, ly) | (lx, linha) <- zip [0..] (mapaEstado e), 
                                  (ly, terreno) <- zip [0..] linha, terreno == Terra]
        
        terrasAlinhadas = [p | p@(lx, ly) <- todasTerras, lx == x || ly == y]

        aguasNoMapa = [(lx, ly) | (lx, linha) <- zip [0..] (mapaEstado e), 
                                  (ly, terreno) <- zip [0..] linha, terreno == Agua]

    inimigosVivos = [m | (i, m) <- zip [0..] minhocas, i /= idx, vidaMinhoca m /= Morta]
    posicaoInimigo m = fromMaybe (0,0) (posicaoMinhoca m)

    -- Função de direção robusta para navegação
    dirPara (ax, ay) (cx, cy)
        | ay > cy = Este
        | ay < cy = Oeste
        | ax < cx = Norte
        | ax > cx = Sul
        | otherwise = Norte