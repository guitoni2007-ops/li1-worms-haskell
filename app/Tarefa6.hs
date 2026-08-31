module Tarefa6 where

import Labs2025
import Tarefa0_geral

-- | Estrutura de dados que centraliza as estatísticas de fim de jogo.
data Relatorio = Relatorio  
    { vencedor       :: String  -- ^ Nome ou identificador do vencedor.
    , pontosVenc     :: Int     -- ^ Pontuação final calculada por fórmula própria.
    , tituloP1       :: String  -- ^ Título honorífico do Jogador 1.
    , tituloP2       :: String  -- ^ Título honorífico do Jogador 2.
    , explosivosP1   :: Int     -- ^ Total de bombas gastas pelo P1.
    , explosivosP2   :: Int     -- ^ Total de bombas gastas pelo P2.
    , terraDestruida :: Int     -- ^ Quantidade de blocos de terra removidos do mapa.
    } deriving (Show)

-- | Função principal da Tarefa 6 que gera o relatório comparativo.
gerarRelatorio :: Estado -> Estado -> Relatorio
gerarRelatorio inicial final =
    let
        -- 1. Identificação das Minhocas
        [m1i, m2i] = take 2 (minhocasEstado inicial)
        [m1f, m2f] = take 2 (minhocasEstado final)

        -- 2. Extração de HP
        hp1 = extraiHP (vidaMinhoca m1f)
        hp2 = extraiHP (vidaMinhoca m2f)

        -- 3. Cálculo de Gastos (Diferencial de Inventário)
        j1 = jetpackMinhoca m1i - jetpackMinhoca m1f
        e1 = escavadoraMinhoca m1i - escavadoraMinhoca m1f
        x1 = (minaMinhoca m1i - minaMinhoca m1f) + (dinamiteMinhoca m1i - dinamiteMinhoca m1f)

        j2 = jetpackMinhoca m2i - jetpackMinhoca m2f
        e2 = escavadoraMinhoca m2i - escavadoraMinhoca m2f
        x2 = (minaMinhoca m2i - minaMinhoca m2f) + (dinamiteMinhoca m2i - dinamiteMinhoca m2f)

        -- 4. Lógica de Títulos
        darTitulo j e x = 
            if j >= e && j >= x && j > 0 then "O Nomada"
            else if e >= j && e >= x && e > 0 then "O Arqueologo"
            else if x > 0 then "O Demolidor"
            else "O Pacifista"

        -- 5. Análise de Impacto no Mapa
        contarTerra m = length [t | linha <- mapaEstado m, t <- linha, t == Terra]
        terraPerdida = contarTerra inicial - contarTerra final

        -- 6. Atribuição de Vitória e Pontuação
        (venc, pts) | hp1 > 0 && hp2 <= 0 = ("Jogador 1", (100 - hp2) * 10 + hp1)
                    | hp2 > 0 && hp1 <= 0 = ("Jogador 2", (100 - hp1) * 10 + hp2)
                    | otherwise           = ("Empate", 1000)

    in Relatorio {
        vencedor       = venc,
        pontosVenc     = pts,
        tituloP1       = darTitulo j1 e1 x1,
        tituloP2       = darTitulo j2 e2 x2,
        explosivosP1   = x1,
        explosivosP2   = x2,
        terraDestruida = terraPerdida
    }

-- | Converte o tipo 'VidaMinhoca' num valor inteiro.
extraiHP :: VidaMinhoca -> Int
extraiHP v = case v of { Viva h -> h; Morta -> 0 }

-- | Prepara uma string formatada com os dados do vencedor.
mostraVencedor :: Relatorio -> String
mostraVencedor r = "Vencedor: " ++ vencedor r ++ " (" ++ show (pontosVenc r) ++ " pts)"