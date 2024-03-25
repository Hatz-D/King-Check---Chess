-- Diogo Lourenzon Hatz 
-- Nicolas Fernandes Melnik 

-- main
xeque [a, b, c, d, e, f, g, h] = iteradorMaior lista 1 lista
    where
        lista = [a, b, c, d, e, f, g, h]

-- função que acha o rei branco

achaRei (a:b) x = achaReiMenor a b x 1 

achaReiMenor [] c x y = achaRei c (x+1)
achaReiMenor (a:b) c x y 
    | a == 'R' = [x,y]
    | a == '1' = achaReiMenor b c x (y+1)
    | a == '2' = achaReiMenor b c x (y+2)
    | a == '3' = achaReiMenor b c x (y+3)
    | a == '4' = achaReiMenor b c x (y+4)
    | a == '5' = achaReiMenor b c x (y+5)
    | a == '6' = achaReiMenor b c x (y+6)
    | a == '7' = achaReiMenor b c x (y+7)
    | a == '8' = achaReiMenor b c x (y+8)
    | y >= 8 = achaRei c (x+1)
    | otherwise = achaReiMenor b c x (y+1) 

-- função que acha o rei branco em uma linha

achaReiEmLinha (a:b) y
    | a == 'R' = y
    | a == '1' = achaReiEmLinha b (y+1)
    | a == '2' = achaReiEmLinha b (y+2)
    | a == '3' = achaReiEmLinha b (y+3)
    | a == '4' = achaReiEmLinha b (y+4)
    | a == '5' = achaReiEmLinha b (y+5)
    | a == '6' = achaReiEmLinha b (y+6)
    | a == '7' = achaReiEmLinha b (y+7)
    | a == '8' = achaReiEmLinha b (y+8)
    | otherwise = achaReiEmLinha b (y+1)

-- iterador maior, sob a lista de listas

iteradorMaior [] x lista = False
iteradorMaior (a:b) x lista = iterador a x 1 lista || iteradorMaior b (x+1) lista

-- iterador menor, sob a lista de chars

iterador [] x y lista = False
iterador (a:b) x y lista
    | a == 'p' = comePeao x y (achaRei lista 1) || iterador b x (y+1) lista
    | a == 't' = comeTorre x y (achaRei lista 1) lista || iterador b x (y+1) lista
    | a == 'c' = comeCavalo x y (achaRei lista 1) || iterador b x (y+1) lista
    | a == 'b' = comeBispo x y (achaRei lista 1) lista || iterador b x (y+1) lista
    | a == 'd' = comeDama x y (achaRei lista 1) lista|| iterador b x (y+1) lista
    | a == '1' = iterador b x (y+1) lista
    | a == '2' = iterador b x (y+2) lista
    | a == '3' = iterador b x (y+3) lista
    | a == '4' = iterador b x (y+4) lista
    | a == '5' = iterador b x (y+5) lista
    | a == '6' = iterador b x (y+6) lista
    | a == '7' = iterador b x (y+7) lista
    | a == '8' = iterador b x (y+8) lista
    | otherwise = iterador b x (y+1) lista

-- função que verifica se determinado cavalo pode fazer xeque

comeCavalo x y [m,n]
    | x == m-2 && (y == n-1 || y == n+1) = True
    | (x == m-1 || x == m+1) && y == n-2 = True
    | (x == m-1 || x == m+1) && y == n+2 = True
    | x == m+2 && (y == n-1 || y == n+1) = True
    | otherwise = False

-- função que verifica se determinado peão pode fazer xeque

comePeao x y [m,n]
    | x == m-1 && (y == n-1 || y == n+1) = True
    | otherwise = False 
 
-- função que verifica se determinado bispo pode fazer xeque

comeBispo x y [m,n] lista
    | abs (x-m) == abs (y-n) && ((x > m && y < n) || (x < m && y > n)) = bloqueiaDiagonalBarra lista x y m n 
    | abs (x-m) == abs (y-n) && ((x > m && y > n) || (x < m && y < n)) = bloqueiaDiagonalInvertida lista x y m n
    | otherwise = False

-- função que verifica se determinada rainha pode fazer xeque

comeDama x y [m,n] lista
    | x == m = bloqueiaHorizontal lista x y m n 1 
    | y == n = bloqueiaVertical lista x y m n 1
    | abs (x-m) == abs (y-n) && ((x > m && y < n) || (x < m && y > n)) = bloqueiaDiagonalBarra lista x y m n 
    | abs (x-m) == abs (y-n) && ((x > m && y > n) || (x < m && y < n)) = bloqueiaDiagonalInvertida lista x y m n
    | otherwise = False

-- função que verifica se determinada torre pode fazer xeque

comeTorre x y [m,n] lista
    | x == m = bloqueiaHorizontal lista x y m n 1 
    | y == n = bloqueiaVertical lista x y m n 1
    | otherwise = False

-- função que verifica se tem uma peça na frente horizontalmente

bloqueiaHorizontal [] x y m n i = False
bloqueiaHorizontal (a:b) x y m n i 
    | i == x = horizontalMenor a x y m n 1
    | otherwise = bloqueiaHorizontal b x y m n (i+1)

horizontalMenor [] x y m n j = False
horizontalMenor (a:b) x y m n j
    | j >= y && j >= n = True
    | ((j > y && j < n) || (j < y && j > n)) && (a == 't' || a == 'c' || a == 'b' || a == 'd' || a == 'r' || a == 'p' || a == 'T' || a == 'C' || a == 'B' || a == 'D' || a == 'P') = False
    | a == '1' = horizontalMenor b x y m n (j+1)
    | a == '2' = horizontalMenor b x y m n (j+2)
    | a == '3' = horizontalMenor b x y m n (j+3)
    | a == '4' = horizontalMenor b x y m n (j+4)
    | a == '5' = horizontalMenor b x y m n (j+5)
    | a == '6' = horizontalMenor b x y m n (j+6)
    | a == '7' = horizontalMenor b x y m n (j+7)
    | a == '8' = horizontalMenor b x y m n (j+8)
    | (j <= y && j <= n) || j == y || j == n = horizontalMenor b x y m n (j+1)

-- função que verifica se tem uma peça na frente verticalmente. transforma a vertical em uma horizontal e reutiliza a função de verificar se tem peças na horizontal

bloqueiaVertical (a:b) x y m n i = horizontalMenor (mapFix (mapVerticalHorizontal (a:b) y) 0) y x n m 1

-- função que verifica se tem uma peça na frente diagonalmente. transforma a diagonal em uma horizontal e reutiliza a função de verificar se tem peças na horizontal

bloqueiaDiagonalBarra (a:b) x y m n = horizontalMenor (mapFix (mapDiagonalHorizontal (a:b) i j 1) 0) 0 ((aabs (y-j))+1) 0 k 1
    where
        dupla = limiteBarra x y
        i = pri dupla
        j = seg dupla
        k = achaReiEmLinha (mapFix (mapDiagonalHorizontal (a:b) (pri (limiteBarra x y)) (seg (limiteBarra x y)) 1) 0) 1

-- função que verifica se tem uma peça na frente diagonalmente. transforma a diagonal em uma horizontal e reutiliza a função de verificar se tem peças na horizontal

bloqueiaDiagonalInvertida (a:b) x y m n = horizontalMenor (mapFix (mapDiagonalInvertidaHorizontal (a:b) i j 1) 0) 0 ((aabs (y-j)+1)) 0 k 1
    where
        dupla = limiteBarraInvertida x y
        i = pri dupla
        j = seg dupla
        k = achaReiEmLinha (mapFix (mapDiagonalInvertidaHorizontal (a:b) (pri (limiteBarraInvertida x y)) (seg (limiteBarraInvertida x y)) 1) 0) 1

-- função que mapeia uma coluna do tabuleiro para uma lista. para fazer isso, primeiro mapeia os elementos considerando números como 1s e depois agrupa os 1s em números maiores com a função mapFix

mapVerticalHorizontal [] y = []
mapVerticalHorizontal (a:b) y = (mapAux a y 1):(mapVerticalHorizontal b y)

mapAux [] y j = '1'
mapAux (a:b) y j 
    | a == '1' = mapAux b y (j+1)
    | a == '2' = mapAux b y (j+2)
    | a == '3' = mapAux b y (j+3)
    | a == '4' = mapAux b y (j+4)
    | a == '5' = mapAux b y (j+5)
    | a == '6' = mapAux b y (j+6)
    | a == '7' = mapAux b y (j+7)
    | a == '8' = mapAux b y (j+8)
    | j == y = a
    | j > y = '1'
    | otherwise = mapAux b y (j+1)

mapFix [] i
    | i == 0 = []
    | i == 1 = ['1']
    | i == 2 = ['2']
    | i == 3 = ['3']
    | i == 4 = ['4']
    | i == 5 = ['5']
    | i == 6 = ['6']
    | i == 7 = ['7']
    | i == 8 = ['8']

mapFix (a:b) i
    | a == '1' = mapFix b (i+1)
    | i == 0 = a:mapFix b 0
    | i == 1 = '1':a:mapFix b 0
    | i == 2 = '2':a:mapFix b 0
    | i == 3 = '3':a:mapFix b 0
    | i == 4 = '4':a:mapFix b 0
    | i == 5 = '5':a:mapFix b 0
    | i == 6 = '6':a:mapFix b 0
    | i == 7 = '7':a:mapFix b 0
    | i == 8 = '8':a:mapFix b 0

-- função que mapeia uma diagonal do tabuleiro para uma lista. para fazer isso, primeiro mapeia os elementos considerando números como 1s e depois agrupa os 1s em números maiores com a função mapFix

mapDiagonalHorizontal [] x y i = []
mapDiagonalHorizontal (a:b) x y i
    | x > i = mapDiagonalHorizontal b x y (i+1)
    | y <= 0 || x >= 9 = []
    | otherwise = (mapAux2 a x y 1):(mapDiagonalHorizontal b (x+1) (y-1) (i+1))

mapAux2 [] x y j = '1'
mapAux2 (a:b) x y j
    | a == '1' = mapAux2 b x y (j+1)
    | a == '2' = mapAux2 b x y (j+2)
    | a == '3' = mapAux2 b x y (j+3)
    | a == '4' = mapAux2 b x y (j+4)
    | a == '5' = mapAux2 b x y (j+5)
    | a == '6' = mapAux2 b x y (j+6)
    | a == '7' = mapAux2 b x y (j+7)
    | a == '8' = mapAux2 b x y (j+8)
    | j == y = a
    | j > y = '1'
    | otherwise = mapAux2 b x y (j+1)

-- função que mapeia uma diagonal do tabuleiro para uma lista. para fazer isso, primeiro mapeia os elementos considerando números como 1s e depois agrupa os 1s em números maiores com a função mapFix

mapDiagonalInvertidaHorizontal [] x y i = []
mapDiagonalInvertidaHorizontal (a:b) x y i
    | x > i = mapDiagonalInvertidaHorizontal b x y (i+1) 
    | y >= 9 || x >= 9 = []
    | otherwise = (mapAux3 a x y 1):(mapDiagonalInvertidaHorizontal b (x+1) (y+1) (i+1))

mapAux3 [] x y j = '1'
mapAux3 (a:b) x y j
    | a == '1' = mapAux3 b x y (j+1)
    | a == '2' = mapAux3 b x y (j+2)
    | a == '3' = mapAux3 b x y (j+3)
    | a == '4' = mapAux3 b x y (j+4)
    | a == '5' = mapAux3 b x y (j+5)
    | a == '6' = mapAux3 b x y (j+6)
    | a == '7' = mapAux3 b x y (j+7)
    | a == '8' = mapAux3 b x y (j+8)
    | j == y = a
    | j > y = '1'
    | otherwise = mapAux3 b x y (j+1)

-- função que acha o limite inferior do tabuleiro para uma diagonal barra

limiteBarra x y
    | x <= 1 || y >= 8 = [x,y]
    | otherwise = limiteBarra (x-1) (y+1)

-- função que acha o limite superior do tabuleiro para uma diagonal barra

limiteBarraInvertida x y
    | x <= 1 || y <= 1 = [x,y]
    | otherwise = limiteBarraInvertida (x-1) (y-1)

-- função que calcula o absoluto de um número

aabs x
    | x >= 0 = x
    | otherwise = -x

-- função para escolher primeiro ou escolher segundo
pri [a,b] = a
seg [a,b] = b