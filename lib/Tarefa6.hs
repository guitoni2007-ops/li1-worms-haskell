module Tarefa6 where

import Labs2025
import Tarefa0_geral

-- | Estrutura que guarda todos os resultados do jogo
data Relatorio = Relatorio {
    vencedor       :: String,
    pontosVenc     :: Int,
    tituloP1       :: String,
    tituloP2       :: String,
    explosivosP1   :: Int,
    explosivosP2   :: Int,
    terraDestruida :: Int
} deriving (Show)

-- | Função principal que o teu colega deve chamar no fim do jogo
gerarRelatorio :: Estado -> Estado -> Relatorio
gerarRelatorio inicial final =
    let
        -- 1. Identificar as minhocas (Iniciais e Finais)
        [m1i, m2i] = take 2 (minhocasEstado inicial)
        [m1f, m2f] = take 2 (minhocasEstado final)

        -- 2. Extrair HP atual para lógica de vitória
        hp1 = extraiHP (vidaMinhoca m1f)
        hp2 = extraiHP (vidaMinhoca m2f)

        -- 3. Calcular Gastos de munições (Início - Fim)
        j1 = jetpackMinhoca m1i - jetpackMinhoca m1f
        e1 = escavadoraMinhoca m1i - escavadoraMinhoca m1f
        x1 = (minaMinhoca m1i - minaMinhoca m1f) + (dinamiteMinhoca m1i - dinamiteMinhoca m1f)

        j2 = jetpackMinhoca m2i - jetpackMinhoca m2f
        e2 = escavadoraMinhoca m2i - escavadoraMinhoca m2f
        x2 = (minaMinhoca m2i - minaMinhoca m2f) + (dinamiteMinhoca m2i - dinamiteMinhoca m2f)

        -- 4. Atribuição de Títulos com base no maior gasto
        darTitulo j e x = 
            if j >= e && j >= x && j > 0 then "O Nomada"
            else if e >= j && e >= x && e > 0 then "O Arqueologo"
            else if x > 0 then "O Demolidor"
            else "O Pacifista"

        -- 5. Contagem de destruição de Terra
        contarTerra m = length [t | linha <- mapaEstado m, t <- linha, t == Terra]
        terraPerdida = contarTerra inicial - contarTerra final

        -- 6. Vencedor e Cálculo de Pontos (A tua fórmula)
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

-- | Função auxiliar para o HP
extraiHP :: VidaMinhoca -> Int
extraiHP v = case v of { Viva h -> h; Morta -> 0 }

-- | Devolve a frase pronta para o ecrã (Ex: "Vencedor: Jogador 1 (1080 pts)")
mostraVencedor :: Relatorio -> String
mostraVencedor r = "Vencedor: " ++ vencedor r ++ " (" ++ show (pontosVenc r) ++ " pts)"
