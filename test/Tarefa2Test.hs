module Main where

import Labs2025
import Tarefa2
import Magic

---cabal clean && rm -rf t2-feedback.tix
---cabal run --enable-coverage t2-feedback
--- ./runhpc.sh t2-feedback

minhocaFullMuni :: Posicao -> VidaMinhoca -> Minhoca
minhocaFullMuni pos vida = Minhoca
    { posicaoMinhoca = Just pos
    , vidaMinhoca = vida
    , jetpackMinhoca = 10
    , escavadoraMinhoca = 10
    , bazucaMinhoca = 10
    , minaMinhoca = 10
    , dinamiteMinhoca = 10
    }

minhocaNoMuni :: Posicao -> VidaMinhoca -> Minhoca
minhocaNoMuni pos vida = (minhocaFullMuni pos vida)
    { jetpackMinhoca = 0
    , escavadoraMinhoca = 0
    , bazucaMinhoca = 0
    , minaMinhoca = 0
    , dinamiteMinhoca = 0
    }

-- Mapa 6x6 Final: 3 Ar, 1 Terra/Agua, 2 Pedra
mapaFinal =
    [ [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 0
    , [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 1
    , [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 2
    , [Terra, Terra, Terra, Terra, Agua, Agua] -- Linha 3
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra] -- Linha 4
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra] -- Linha 5
    ]

-- ESTADOS BASE FINAIS
-- M0 em (1,1) (No Ar)
-- M1 em (2,1) (No Chão, pois (3,1) é Terra)
estadoBaseFinal = Estado
    { mapaEstado = mapaFinal
    , objetosEstado = []
    , minhocasEstado =
        [ minhocaFullMuni (1,1) (Viva 100) -- M0
        , minhocaFullMuni (2,1) (Viva 100) -- M1
        ]
    }

-- Estado para Teste de Colisão da Escavadora/Mina/Dinamite
estadoColisao = estadoBaseFinal
    { minhocasEstado =
        [ minhocaFullMuni (2,1) (Viva 100) -- M0 (Minhoca de baixo)
        , minhocaFullMuni (1,1) (Viva 100) -- M1 (Minhoca de cima, bloqueia o Norte de M0)
        ]
    }

-- Estado para Teste de Movimento (M1 no chão, M0 no ar)
estadoMove = estadoBaseFinal
    { mapaEstado =
        [ [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Agua, Agua]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , minhocasEstado =
        [ minhocaFullMuni (2,1) (Viva 100) -- M0 (No Ar, vai cair)
        , minhocaFullMuni (3,1) (Viva 100) -- M1 (No Chão, em cima da Terra)
        ]
    }

-- NOVO ESTADO INTEGRADO: Morte por Água
mapaAgua =
    [ [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 0
    , [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 1
    , [Ar, Ar, Ar, Ar, Ar, Ar] -- Linha 2
    , [Terra, Terra, Terra, Agua, Agua, Agua] -- Linha 3
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra] -- Linha 4
    , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra] -- Linha 5
    ]

estadoMorteAgua = Estado
    { mapaEstado = mapaAgua
    , objetosEstado = []
    , minhocasEstado =
        [ minhocaFullMuni (2,3) (Viva 100) -- M0 (Posição de partida)
        ]
    }

-- NOVO ESTADO PARA O TESTE (2,2) -> SUDESTE
estadoMorteAgua2 = estadoMorteAgua
    { minhocasEstado =
        [ minhocaFullMuni (2,2) (Viva 100) -- M0 (Posição de partida)
        ]
    }

testesDisparo :: [(Int, Jogada, Estado)]
testesDisparo =
    [ -- COBERTURA DE CONDIÇÃO (Linhas 114 e 115)
      (0, Dispara Bazuca Sul, estadoBaseFinal { minhocasEstado = [minhocaNoMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] }) -- Sem Munição (Bazuca)
    , (0, Dispara Bazuca Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) Morta, minhocaFullMuni (2,1) (Viva 100)] }) -- Minhoca Morta

      -- BAZUCA (Teste básico)
    , (0, Dispara Bazuca Sul, estadoBaseFinal) -- M0 dispara para (2,1)

      -- JETPACK (Lógica: Bloqueado -> Gasta munição, não move; 
      -- 1. Bloqueado por Terra (Gasta munição, não move)
    , (1, Dispara Jetpack Sul, estadoBaseFinal) -- M1 em (2,1) tenta ir para (3,1) (Terra)
      -- 2. Bloqueado por Entidade (Gasta munição, não move)
    , (0, Dispara Jetpack Norte, estadoColisao) -- M0 em (2,1) tenta ir para (1,1) (ocupado por M1) 
      -- 3. Livre (Move)
    , (0, Dispara Jetpack Este, estadoBaseFinal) -- M0 em (1,1) move para (1,2)

      -- ESCAVADORA (Lógica: Colisão/Pedra/Fora 
      -- 1. Colisão (Gasta munição, não move)
    , (0, Dispara Escavadora Norte, estadoColisao) -- M0 em (2,1) tenta ir para (1,1) (ocupado por M1)
      -- 2. Pedra (Gasta munição, não move)
    , (1, Dispara Escavadora Sul, estadoBaseFinal) -- M1 em (2,1) tenta ir para (3,1) (Terra), depois (4,1) (Pedra) 
      -- 3. Terra (Move e destrói)
    , (1, Dispara Escavadora Sul, estadoBaseFinal) -- M1 em (2,1) move para (3,1) (Terra) 

      -- MINA (Lógica: Bloqueado -> Posição atual; Livre -> Destino)
      -- 1. Bloqueado por Terra (Posição atual)
    , (1, Dispara Mina Sul, estadoBaseFinal) -- M1 em (2,1) tenta ir para (3,1) (Terra) 
      -- 2. Livre (Destino)
    , (0, Dispara Mina Este, estadoBaseFinal) -- M0 em (1,1) move para (1,2) (Ar)

      -- DINAMITE (Lógica: Bloqueado -> Posição atual; Livre -> Destino)
      -- 1. Bloqueado por Terra (Posição atual)
    , (1, Dispara Dinamite Sul, estadoBaseFinal) -- M1 em (2,1) tenta ir para (3,1) (Terra)
      -- 2. Livre (Destino)
    , (0, Dispara Dinamite Este, estadoBaseFinal) -- M0 em (1,1) move para (1,2) (Ar) 
    ]

-- ============================================================================
-- NOVOS TESTES 
-- ============================================================================

testesMove :: [(Int, Jogada, Estado)]
testesMove =
    [ -- MOVIMENTO BÁSICO (Move para Ar)
      (1, Move Norte, estadoMove) -- M1 em (3,1) move para (2,1) (Ar)

      -- MOVIMENTO BLOQUEADO (Move para Terra/Pedra)
    , (1, Move Sul, estadoMove) -- M1 em (3,1) tenta ir para (4,1) (Pedra) 

      -- MOVIMENTO BLOQUEADO (Move para Entidade)
    , (0, Move Sul, estadoColisao) -- M0 em (2,1) tenta ir para (3,1) (ocupado por M1) 

      -- QUEDA (Morte por Queda)
    , (0, Move Norte, estadoMove) -- M0 em (2,1) move para (1,1) (Ar), depois cai para (3,1) (Terra)

      -- QUEDA (Morte por Água (Teste 1: (2,3) -> Sudeste))
    , (0, Move Sudeste, estadoMorteAgua) -- M0 em (2,3) move para (3,4) (Água) -

      -- QUEDA (Morte por Água (Teste 2: (2,2) -> Sudeste))
    , (0, Move Sudeste, estadoMorteAgua2) -- M0 em (2,2) move para (3,3) (Água) 
    
    -- Movimento ignorado por minhoca morta
    , (0, Move Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) Morta, minhocaFullMuni (2,1) (Viva 100)] })

    -- Disparo para fora do mapa (Norte de linha 0)
    ,  (0, Dispara Bazuca Norte, estadoBaseFinal)

    -- Movimento para fora do mapa (Norte de linha 0)
    ,  (0, Move Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (0,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

     -- Jetpack sem munição
    ,  (0, Dispara Jetpack Este, estadoBaseFinal { minhocasEstado = [minhocaNoMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Sudeste bloqueado por Pedra
    ,  (0, Move Sudeste, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (3,4) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Mina bloqueada por outra minhoca
    ,  (0, Dispara Mina Sul, estadoColisao)

    -- Mina bloqueada por outra minhoca
    ,  (0, Dispara Mina Sul, estadoColisao)

    -- Queda segura em Pedra
    ,  (0, Move Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (2,1) (Viva 100), minhocaFullMuni (3,1) (Viva 100)] })

    -- Minhoca com vida 0 (deve ser considerada morta)
    ,  (0, Move Este, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) (Viva 0), minhocaFullMuni (2,1) (Viva 100)] })

    -- Minhoca explicitamente Morta
    ,  (0, Move Este, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) Morta, minhocaFullMuni (2,1) (Viva 100)] })

    -- Todas mortas
    ,  (0, Move Este, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) Morta, minhocaFullMuni (2,1) Morta] })

    -- Movimento para posição fora da matriz (linha negativa)
    ,  (0, Move Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (0,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Movimento para posição fora da matriz (coluna negativa)
    ,  (0, Move Oeste, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,0) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Movimento para posição ocupada por outra minhoca
    ,  (0, Move Sul, estadoColisao) -- M0 tenta ir para (3,1), onde está M1

    -- Movimento para posição válida e livre
    ,  (0, Move Este, estadoBaseFinal) -- M0 em (1,1) move para (1,2)

        -- Morte por água (movimento para posição de Água)
    , (0, Move Sudeste, estadoMorteAgua) -- M0 em (2,3) move para (3,4) (Água) 

    -- Morte por água (posição diferente)
    , (0, Move Sudeste, estadoMorteAgua2) -- M0 em (2,2) move para (3,3) (Água) 

    -- Queda longa que termina em Água (morte)
    , (0, Move Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,4) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Queda em Água a partir do topo
    , (0, Move Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (0,3) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Queda em Água por movimento diagonal
    , (0, Move Sudeste, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (2,2) (Viva 100), minhocaFullMuni (3,1) (Viva 100)] })

    -- Queda em Água por movimento vertical
    , (0, Move Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (2,4) (Viva 100), minhocaFullMuni (3,1) (Viva 100)] })
    
    -- Minhoca morta tenta disparar bazuca
    ,  (0, Dispara Bazuca Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) Morta, minhocaFullMuni (2,1) (Viva 100)] })

    -- Minhoca viva sem munição de bazuca
    ,  (0, Dispara Bazuca Sul, estadoBaseFinal { minhocasEstado = [minhocaNoMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    ,(0, Dispara Bazuca Sul, estadoBaseFinal
  { minhocasEstado =
      [ minhocaFullMuni (1,1) Morta  -- morta mas com munição
      , minhocaFullMuni (2,1) (Viva 100)
      ]
  })

    ,(0, Dispara Bazuca Sul, estadoBaseFinal
  { minhocasEstado =
      [ minhocaNoMuni (1,1) (Viva 100)  -- sem munição
      , minhocaFullMuni (2,1) (Viva 100)
      ]
  })
     -- M1 em (2,1) move para (3,1) (Terra) 
    ,(1, Dispara Escavadora Sul, estadoBaseFinal)
    
         -- Escavadora move para Ar (usa moveEscavadora)
    , (0, Dispara Escavadora Este, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Escavadora move para Água (usa moveEscavadora)
    , (0, Dispara Escavadora Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (2,4) (Viva 100), minhocaFullMuni (3,1) (Viva 100)] })

    -- Escavadora tenta mover para Pedra (usa gastaMunicaoEscavadora)
    , (0, Dispara Escavadora Sul, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (3,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

    -- Escavadora tenta mover para fora do mapa (linha inválida)
    , (0, Dispara Escavadora Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (0,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })
    
    -- Jetpack bloqueado por entidade
    , (0, Dispara Jetpack Sul, estadoColisao)

    -- Jetpack bloqueado por Terra
    , (1, Dispara Jetpack Sul, estadoBaseFinal)

    -- Jetpack livre para mover
    , (0, Dispara Jetpack Este, estadoBaseFinal)

        -- Bazuca com minhoca válida
    , (0, Dispara Bazuca Este, estadoBaseFinal)

    ,(0, Dispara Bazuca Este, estadoBaseFinal
  { minhocasEstado =
      [ (minhocaFullMuni (1,1) (Viva 100)) { posicaoMinhoca = Nothing }
      , minhocaFullMuni (2,1) (Viva 100)
      ]
  })

    -- Bazuca com minhoca não encontrada (índice inválido)
    , (3, Dispara Bazuca Este, estadoBaseFinal)
    
        -- Mina bloqueada por entidade (fica na posição atual)
    , (0, Dispara Mina Sul, estadoColisao)

    -- Mina livre (vai para destino)
    , (0, Dispara Mina Este, estadoBaseFinal)

    -- Mina com barril na posição destino
    , (0, Dispara Mina Este, estadoBaseFinal { objetosEstado = [Barril (1,2) False ], minhocasEstado = [minhocaFullMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })
    
    -- Jetpack leva minhoca para fora do mapa (morte)
    , (0, Dispara Jetpack Norte, estadoBaseFinal { minhocasEstado = [minhocaFullMuni (0,0) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })

        -- Escavadora para fora do mapa (linha negativa)
    , (0, Dispara Escavadora Norte, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (0,1) (Viva 100)
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

    -- Escavadora para fora do mapa (coluna negativa)
    , (0, Dispara Escavadora Oeste, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (1,0) (Viva 100)
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

    -- Escavadora para fora do mapa (linha além do limite)
    , (0, Dispara Escavadora Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (4,1) (Viva 100)  -- supondo mapa 5x5
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

    -- Escavadora para fora do mapa (coluna além do limite)
    , (0, Dispara Escavadora Este, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (1,4) (Viva 100)  -- supondo mapa 5x5
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

        -- Escavadora para posiçao ocupada por outra minhoca
    , (0, Dispara Escavadora Sul, estadoColisao) -- M0 tenta ir para (3,1), onde está M1

    -- Escavadora para célula de Pedra
    , (0, Dispara Escavadora Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (3,1) (Viva 100)
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

    -- Escavadora para célula de Terra (deve escavar e mover)
    , (0, Dispara Escavadora Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (2,1) (Viva 100)
            , minhocaFullMuni (3,1) (Viva 100)
            ]
        })

    -- Escavadora para célula de Ar (deve mover sem escavar)
    , (0, Dispara Escavadora Este, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (1,1) (Viva 100)
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })
    -- Escavadora para célula de Ar (deve mover sem escavar)
    , (0, Dispara Escavadora Nordeste, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (1,1) (Viva 100)
            , minhocaFullMuni (2,1) (Viva 100)
            ]
        })
    -- Escavadora para célula de Água (não deve mover)
    , (0, Dispara Escavadora Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (2,4) (Viva 100)
            , minhocaFullMuni (3,1) (Viva 100)
            ]
        })

    -- M0 em (1,1), tenta Jetpack Sul para (2,1), que está ocupado por M1
    , (0, Dispara Jetpack Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (1,1) (Viva 100)
      , minhocaFullMuni (2,1) (Viva 100)
            ]
        })

    -- M0 em (1,1), Jetpack Este para (1,2),
    ,  (0, Dispara Jetpack Este, estadoBaseFinal
       { minhocasEstado =
            [ minhocaFullMuni (1,1) (Viva 100)
      , minhocaFullMuni (3,3) (Viva 100)
            ]
       })

    -- Jogada não é Jetpack → deve cair em podeDisparoJetpack 
    ,   (0, Dispara Bazuca Sul, estadoBaseFinal)
  
    -- M0 em (2,4), escavadora Sul para (3,4), que é Água
    ,   (0, Dispara Escavadora Sul, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (2,4) (Viva 100)
      , minhocaFullMuni (3,1) (Viva 100)
            ]
        })

     -- M0 em (0,0), escavadora Norte para (-1,0) → posição inválida
    ,   (0, Dispara Escavadora Norte, estadoBaseFinal
        { minhocasEstado =
            [ minhocaFullMuni (0,0) (Viva 100)
      , minhocaFullMuni (2,1) (Viva 100)
            ]
        })
    
    , (0, Dispara Jetpack Norte, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })
     
    , (0, Dispara Bazuca Sul, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (1,1) (Viva 100)]
        , objetosEstado = []
       }) -- depois remove a minhoca do estado e tenta encontrá-la
    
    ,  (0, Dispara Bazuca Sul, estadoBaseFinal
       { minhocasEstado = [(minhocaFullMuni (1,1) (Viva 100)) { posicaoMinhoca = Nothing }] })

    ,  (0, Dispara Bazuca Sul, estadoBaseFinal
       { minhocasEstado = [(minhocaFullMuni (1,1) (Viva 100)) { posicaoMinhoca = Nothing }] })
    
    ,  (3, Dispara Mina Sul, estadoBaseFinal)
    
    ,  (0, Dispara Mina Este, estadoBaseFinal
       { objetosEstado = [Barril (1,2) True]
       , minhocasEstado = [minhocaFullMuni (1,1) (Viva 100)] })
     
    ,  (0, Dispara Mina Sul, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (1,1) (Viva 100), minhocaFullMuni (2,1) (Viva 100)] })
    
    ,  (0, Dispara Jetpack Norte, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (0,0) (Viva 100)] })
    
    ,  (0, Dispara Escavadora Sul, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (2,1) (Viva 100)] }) -- (2,1) é Terra
    
    ,  (0, Dispara Escavadora Sul, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (1,1) (Viva 100)] }) -- (1,1) é Ar
    
    ,  (0, Dispara Bazuca Sul, estadoBaseFinal
       { minhocasEstado = [minhocaNoMuni (1,1) (Viva 100)] })
    
    ,  (0, Dispara Bazuca Sul, estadoBaseFinal
       { minhocasEstado = [minhocaFullMuni (1,1) Morta] })


    ]
 
-- COMBINAÇÃO FINAL

jogadasTesteFinal :: [(Int, Jogada, Estado)]
jogadasTesteFinal = testesDisparo ++ testesMove

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = jogadasTesteFinal
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
