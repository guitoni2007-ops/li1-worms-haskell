{-|
Module      : Tarefa0_geral
Description : Funções auxiliares gerais.

Módulo que define funções genéricas sobre listas e matrizes.
-}

module Tarefa0_geral where

-- * Tipos de dados

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/matriz.png>>
type Matriz a = [[a]]

-- | Uma posição numa matriz é dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/posicaomatriz.png>>
type Posicao = (Int,Int)

-- | A dimensão de uma matrix dada como um par (/número de linhas/,/número de colunhas/).
type Dimensao = (Int,Int)

-- | Uma direção é dada pela rosa dos ventos. Ou seja, os 4 pontos cardeais e os 4 pontos colaterais.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rosadosventos.jpg>>
data Direcao = Norte | Nordeste | Este | Sudeste | Sul | Sudoeste | Oeste | Noroeste
    deriving (Eq,Ord,Show,Read,Enum)

-- * Funções não-recursivas.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l = x >= 0 && x < length l


-- | Calcula a dimensão de uma matriz.
--ePosicaoMatrizValida
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0, 0)
dimensaoMatriz m  = (length m, length (head m))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (l,c) m = eIndiceListaValido l m && eIndiceListaValido c (head m)


-- | Move uma posição uma unidade no sentido de uma direção.
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao d (l,c)   | d==Norte = (l-1,c)
                      | d==Sul = (l+1,c)
                      | d==Oeste = (l,c-1)
                      | d==Este = (l,c+1)
                      | d==Nordeste = (l-1,c+1)
                      | d==Noroeste = (l-1,c-1)
                      | d==Sudeste = (l+1,c+1)
                      | d==Sudoeste = (l+1,c-1)
             

-- | Versão da função 'movePosicao' que garante que o movimento não se desloca para fora de uma janela.
--
-- __NB:__ Considere uma janela retangular com origem no canto superior esquerdo definida como uma matriz. A função recebe a dimensao da janela.
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (x,y) d (l,c) | d==Norte && l > 0 = (l-1,c)
                                | d==Sul && l < x-1 = (l+1,c)
                                | d==Oeste && c > 0 = (l,c-1)
                                | d==Este && c < y-1 = (l,c+1)
                                | d==Nordeste && l > 0 && c < y-1 = (l-1,c+1)
                                | d==Noroeste && l > 0 && c > 0 = (l-1,c-1)
                                | d==Sudeste && l < x-1 && c < y-1 = (l+1,c+1)
                                | d==Sudoeste && l < x-1 &&  c > 0 = (l+1,c-1)


-- | Converte uma posição no referencial em que a origem é no canto superior esquerdo da janela numa posição em que a origem passa a estar no centro da janela.
--
-- __NB:__ Considere posições válidas. Efetue arredondamentos como achar necessário.
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (x,y) (l,c) =
  let centroL = x `div` 2
      centroC = y `div` 2
  in (c - centroC, centroL - l)

                        

-- | Roda um par (posição,direção) 45% para a direita.
--
-- __NB:__ Vendo um par (posição,direção) como um vector, cria um novo vetor do desto com a próxima direção da rosa dos ventos rodando para a direita.
--
-- <<https://haslab.github.io/Teaching/LI1/2526/img/rodaposicaodirecao.png>>
rodaPosicaoDirecao :: (Posicao,Direcao) -> (Posicao,Direcao)
rodaPosicaoDirecao ((l,c),d) | d==Norte = ((l,c), Nordeste)
                             | d==Nordeste = ((l,c), Este)
                             | d==Este = ((l,c), Sudeste)
                             | d==Sudeste = ((l,c), Sul)
                             | d==Sul = ((l,c), Sudoeste)
                             | d==Sudoeste = ((l,c), Oeste)
                             | d==Oeste = ((l,c), Noroeste)
                             | d==Noroeste = ((l,c), Norte)
                            

-- * Funções recursivas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __NB:__ Retorna @Nothing@ se o índice não existir.
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista i l
  | i < 0 || i >= length l = Nothing
  | otherwise = Just (l !! i)


-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o índice não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista i x l@(h:t)
  | i < 0     = l                    
  | i == 0    = x : t                 
  | otherwise = h : atualizaIndiceLista (i - 1) x t  


-- | Devolve o elemento numa dada posição de uma matriz.
--
-- __NB:__ Retorna @Nothing@ se a posição não existir.
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (l, c) m
  | l < 0 || l >= length m = Nothing
  | c < 0 || c >= length (m !! l) = Nothing
  | otherwise = Just ((m !! l) !! c)


-- | Modifica um elemento numa dada posição de uma matriz.
--
-- __NB:__ Devolve a própria matriz se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0, c) e (linha:t) = atualizaIndiceLista c e linha : t
atualizaPosicaoMatriz (l, c) e (linha:t) = linha : atualizaPosicaoMatriz (l - 1, c) e t


-- | Aplica uma sequência de movimentações a uma posição, pela ordem em que ocorrem na lista.
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] pos = pos                             
moveDirecoesPosicao (h:t) pos = moveDirecoesPosicao t (movePosicao h pos)


-- | Aplica a mesma movimentação a uma lista de posições.
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes _ [] = []                      
moveDirecaoPosicoes d (h:t) = movePosicao d h : moveDirecaoPosicoes d t


-- | Verifica se uma matriz é válida, no sentido em que modela um rectângulo.
-- __NB:__ Todas as linhas devem ter o mesmo número de colunas.
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = False
eMatrizValida [[]] = False
eMatrizValida [_] = True
eMatrizValida (l1:l2:ls)
  | length l1 == length l2 = eMatrizValida (l2:ls)
  | otherwise = False
