module Main where

import Labs2025
import Tarefa2
import Magic

---cabal clean && rm -rf t2-feedback.tix
---cabal run --enable-coverage t2-feedback
--- ./runhpc.sh t2-feedback

-- MINHOCAS VÁLIDAS
minhocaValida1 = Minhoca{posicaoMinhoca=Just (3,0), vidaMinhoca=Morta, jetpackMinhoca=100, escavadoraMinhoca=200, bazucaMinhoca=150, minaMinhoca=3, dinamiteMinhoca=1} -- posição válida, morta, munições >= 0
minhocaValida2 = Minhoca{posicaoMinhoca=Just (1,1), vidaMinhoca=Viva 50, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva, vida entre 0-100, posição livre
minhocaValida3 = Minhoca{posicaoMinhoca=Nothing, vidaMinhoca=Morta, jetpackMinhoca=10, escavadoraMinhoca=5, bazucaMinhoca=2, minaMinhoca=0, dinamiteMinhoca=0} -- sem posição, obrigatoriamente morta
minhocaValida4 = Minhoca{posicaoMinhoca=Just (2,2), vidaMinhoca=Viva 0, jetpackMinhoca=0, escavadoraMinhoca=0, bazucaMinhoca=0, minaMinhoca=0, dinamiteMinhoca=0} -- viva com vida 0 (permitido), posição válida
minhocaValida5 = Minhoca{posicaoMinhoca=Just (0,0), vidaMinhoca=Viva 100, jetpackMinhoca=50, escavadoraMinhoca=50, bazucaMinhoca=50, minaMinhoca=5, dinamiteMinhoca=2} -- posição válida, vida máxima

-- DISPAROS VÁLIDOS
disparoValido1 = Disparo{posicaoDisparo=(1,4), direcaoDisparo=Norte, tipoDisparo=Mina, tempoDisparo=Just 2, donoDisparo=0}
disparoValido2 = Disparo{posicaoDisparo=(3,2), direcaoDisparo=Sul, tipoDisparo=Dinamite, tempoDisparo=Just 4, donoDisparo=1}
disparoValido3 = Disparo{posicaoDisparo=(5,5), direcaoDisparo=Este, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=2}
disparoValido4 = Disparo{posicaoDisparo=(2,3), direcaoDisparo=Norte, tipoDisparo=Bazuca, tempoDisparo=Nothing, donoDisparo=1} -- perfura terreno opaco se posição anterior não for opaca
disparoValido5 = Disparo{posicaoDisparo=(0,0), direcaoDisparo=Sul, tipoDisparo=Mina, tempoDisparo=Just 0, donoDisparo=3}
disparoValido6 = Disparo{posicaoDisparo=(4,1), direcaoDisparo=Oeste, tipoDisparo=Dinamite, tempoDisparo=Just 2, donoDisparo=0}

-- BARRIS VÁLIDOS
barrilValido1 = Barril{posicaoBarril=(4,6),explodeBarril = False }
barrilValido2 = Barril{posicaoBarril=(0,5),explodeBarril = False }
barrilValido3 = Barril{posicaoBarril=(2,0),explodeBarril = False }

-- MAPA BASE

mapaOk = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
    ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
    ]



estado1valido = Estado
  { mapaEstado = mapaOk
  , objetosEstado = [disparoValido1, disparoValido2, barrilValido1]
  , minhocasEstado = [minhocaValida1, minhocaValida2]
  }

estado2valido = Estado
  { mapaEstado = mapaOk
  , objetosEstado = [disparoValido1, disparoValido2, barrilValido1]
  , minhocasEstado = [minhocaValida1, minhocaValida3]
  }

-- Estado base (válido)
estadoBase = estado1valido

-- Minhoca no ar
estadoNoAr = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (3,0)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 },Minhoca { posicaoMinhoca = Just (3,3)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }]
    }

-- Minhoca morta
estadoMorta = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (2,2)
                 , vidaMinhoca = Morta
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }, minhocaValida2]
    }

-- Minhoca na borda norte (fora do mapa)
estadoBordaNorte = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (0,5)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }, minhocaValida4]
    }

-- Minhoca próxima da água
estadoProxAgua = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (4,8)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }, minhocaValida1]
    }

-- Minhoca sem munições
estadoSemMuni = Estado
    { mapaEstado = mapaOk
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (3,3)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 0
                 , escavadoraMinhoca = 0
                 , bazucaMinhoca = 0
                 , minaMinhoca = 0
                 , dinamiteMinhoca = 0
                 }, minhocaValida2]
    }


estadoComDisparoRepetido = Estado
    { mapaEstado = mapaOk
    , objetosEstado = [Disparo { posicaoDisparo = (3,3)
                               , direcaoDisparo = Sul
                               ,tempoDisparo = Nothing
                               , tipoDisparo = Bazuca
                               , donoDisparo = 0 }]
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (3,3)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 },minhocaValida5]
    }

estadoPedra = Estado
    { mapaEstado = [[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar,Ar]
        ,[Pedra,Pedra,Ar,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar]
        ,[Pedra,Pedra,Pedra,Pedra,Pedra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Ar,Ar,Ar,Ar,Ar]
        ,[Terra,Terra,Terra,Terra,Terra,Pedra,Pedra,Agua,Agua,Agua]
        ]
    , objetosEstado = []
    , minhocasEstado =
        [Minhoca { posicaoMinhoca = Just (2,2)
                 , vidaMinhoca = Viva 100
                 , jetpackMinhoca = 1
                 , escavadoraMinhoca = 1
                 , bazucaMinhoca = 1
                 , minaMinhoca = 1
                 , dinamiteMinhoca = 1
                 }, minhocaValida4]
    }



movimentosTeste =
    [ (1 :: Int, Move Norte, estadoBase)
    , (0 :: Int, Move Sul, estadoBase)
    , (0 :: Int, Move Este, estadoBase)
    , (0 :: Int, Move Oeste, estadoBase)
    , (1 :: Int, Move Nordeste, estadoBase)
    , (0 :: Int, Move Noroeste, estadoBase)
    , (1 :: Int, Move Sudeste, estadoBase)
    , (0 :: Int, Move Sudoeste, estadoBase)
    , (0 :: Int, Move Norte, estadoNoAr)
    , (0 :: Int, Move Norte, estadoBordaNorte)
    , (0 :: Int, Move Este, estadoProxAgua)
    , (1 :: Int, Move Norte, estadoBase)
    , (0 :: Int, Move Oeste, estadoBase)
    , (1 :: Int, Move Sul, estadoBase)
    , (0 :: Int, Move Nordeste, estadoBase)
    , (1 :: Int, Move Noroeste, estadoBase)
    , (0 :: Int, Move Sudeste, estadoBase)
    , (1 :: Int, Move Sudoeste, estadoBase)
    , (0 :: Int, Move Norte, estadoBase)
    , (1 :: Int, Move Sul, estadoBase)
    , (0 :: Int, Move Este, estadoBase)
    , (1 :: Int, Move Oeste, estadoBase)
    , (0 :: Int, Move Nordeste, estadoBase)
    , (1 :: Int, Move Noroeste, estadoBase)
    , (0 :: Int, Move Sudeste, estadoBase)
    , (1 :: Int, Move Sudoeste, estadoBase)
    , (0 :: Int, Move Norte, estadoNoAr)
    , (1 :: Int, Move Sul, estadoNoAr)
    , (0 :: Int, Move Este, estadoNoAr)
    , (1 :: Int, Move Oeste, estadoNoAr)
    , (0 :: Int, Move Nordeste, estadoBordaNorte)
    , (1 :: Int, Move Noroeste, estadoBordaNorte)
    , (0 :: Int, Move Sudeste, estadoBordaNorte)
    , (1 :: Int, Move Sudoeste, estadoBordaNorte)
    , (0 :: Int, Move Norte, estadoProxAgua)
    , (1 :: Int, Move Sul, estadoProxAgua)
    , (0 :: Int, Move Este, estadoProxAgua)
    , (1 :: Int, Move Oeste, estadoProxAgua)
    , (0 :: Int, Move Nordeste, estadoProxAgua)
    , (1 :: Int, Move Noroeste, estadoProxAgua)
    , (0 :: Int, Move Sudeste, estadoProxAgua)
    , (1 :: Int, Move Sudoeste, estadoProxAgua)
    ]



disparosTeste =
    [ (1 :: Int, Dispara Jetpack Norte, estadoBase)
    , (0 :: Int, Dispara Jetpack Nordeste, estadoNoAr)
    , (0 :: Int, Dispara Jetpack Noroeste, estadoBordaNorte)
    , (1 :: Int, Dispara Escavadora Sul, estadoBase)
    , (0 :: Int, Dispara Escavadora Oeste, estadoBase)
    , (0 :: Int, Dispara Escavadora Norte, estadoBordaNorte)
    , (1 :: Int, Dispara Bazuca Sul, estadoBase)
    , (0 :: Int, Dispara Bazuca Noroeste, estadoBordaNorte)
    , (1 :: Int, Dispara Mina Sul, estadoBase)
    , (0 :: Int, Dispara Mina Oeste, estadoBase)
    , (0 :: Int, Dispara Mina Noroeste, estadoBordaNorte)
    , (0 :: Int, Dispara Dinamite Sul, estadoBase)
    , (1 :: Int, Dispara Dinamite Oeste, estadoBase)
    , (0 :: Int, Dispara Dinamite Noroeste, estadoBordaNorte)
    , (1 :: Int, Dispara Jetpack Norte, estadoBase)
    , (1 :: Int, Dispara Jetpack Sul, estadoBase)
    , (1 :: Int, Dispara Jetpack Este, estadoBase)
    , (1 :: Int, Dispara Jetpack Oeste, estadoBase)
    , (1 :: Int, Dispara Jetpack Nordeste, estadoBase)
    , (1 :: Int, Dispara Jetpack Noroeste, estadoBase)
    , (1 :: Int, Dispara Jetpack Sudeste, estadoBase)
    , (1 :: Int, Dispara Jetpack Sudoeste, estadoBase)
    , (1 :: Int, Dispara Escavadora Norte, estadoBase)
    , (1 :: Int, Dispara Escavadora Sul, estadoBase)
    , (1 :: Int, Dispara Escavadora Este, estadoBase)
    , (1 :: Int, Dispara Escavadora Oeste, estadoBase)
    , (1 :: Int, Dispara Bazuca Norte, estadoBase)
    , (1 :: Int, Dispara Bazuca Sul, estadoBase)
    , (1 :: Int, Dispara Bazuca Este, estadoBase)
    , (1 :: Int, Dispara Bazuca Oeste, estadoBase)
    , (1 :: Int, Dispara Bazuca Nordeste, estadoBase)
    , (1 :: Int, Dispara Bazuca Noroeste, estadoBase)
    , (1 :: Int, Dispara Bazuca Sudeste, estadoBase)
    , (1 :: Int, Dispara Bazuca Sudoeste, estadoBase)
    , (1 :: Int, Dispara Mina Norte, estadoBase)
    , (1 :: Int, Dispara Mina Sul, estadoBase)
    , (1 :: Int, Dispara Mina Este, estadoBase)
    , (1 :: Int, Dispara Mina Oeste, estadoBase)
    , (1 :: Int, Dispara Dinamite Norte, estadoBase)
    , (1 :: Int, Dispara Dinamite Sul, estadoBase)
    , (1 :: Int, Dispara Dinamite Este, estadoBase)
    , (1 :: Int, Dispara Dinamite Oeste, estadoBase)
    , (1 :: Int, Dispara Dinamite Nordeste, estadoBase)
    , (1 :: Int, Dispara Dinamite Noroeste, estadoBase)
    , (1 :: Int, Dispara Dinamite Sudeste, estadoBase)
    , (1 :: Int, Dispara Dinamite Sudoeste, estadoBase)
    ]



jogadasTeste :: [(Int, Jogada, Estado)]
jogadasTeste = disparosTeste ++ movimentosTeste



dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = jogadasTeste
    outs <- mapM (\(i,j,e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
