---- questao 1 ---- menor entre dois numeros
min2 :: (Int,Int) -> Int
min2 (x,y) = if x < y then x else y

---- questao 2 ---- menor entre tres numeros
min3 :: (Int,Int,Int) -> Int
min3 (x,y,z) = if x < min2(y,z) then x else min2(y,z)

---- questao 3 ---- fatorial
fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

---- questao 4 ---- fibonacci
fib::Int->Int
fib n
    |n==1 = 0
    |n==2 = 1
    |otherwise = fib(n-1) + fib(n-2)

---- questao 5 ---- dado n, mostrar o n-esimo termo da lista
elemento :: Int -> [a] -> a 
elemento 0 (h:t) = h
elemento x (h:t) = elemento (x-1) t

---- questao 6 ---- se x é elemento da lista
pertence :: (Eq a) => a -> [a] -> Bool
pertence _ [] = False
pertence x (h:t) = if x /= h then pertence x t else True

---- questao 7 ---- tamanho da lista sem usar length
total :: [a] -> Int
total x = sum [1 | _ <- x]

---- questao 8 ---- maior elemento da lista
maior :: Ord a => [a] -> a 
maior [] = undefined 
maior [x] = x
maior (x:y:xs)
    | x > y =  maior(x:xs)
    | otherwise = maior(y:xs)

---- questao 9 ---- retornar quantas vezes x aparece na lista
frequencia :: (Eq a) => a -> [a] -> Int
frequencia _ [] = 0
frequencia n (h:t) = if n == h then 1 + frequencia n t else frequencia n t

---- questao 10 ---- verdadeiro se x ocorre exatamente uma vez
unico :: Int -> [Int] -> Bool
unico _ [] = False
unico n (x:xs)
 | cont n (x:xs) == 1 = True
 | otherwise = False

cont :: Int -> [Int] -> Int
cont _ [] = 0
cont n (x:xs)
 | n == x = 1 + cont n xs
 | otherwise = cont n xs

----- questao 11 --- retornar os valores maiores que x
maioresque :: Int -> [Int] -> [Int]
maioresque _ [] = []
maioresque y xs = [x | x <- xs, x>y]

---- questao 12 ---- concatenacao entre lista a e b
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena a b = a++b

---- questao 13 ---- calda da lista (sem o primeiro elemento)
calda :: [a] -> [a]
calda [] = []
calda (x:xs) = xs

---- questao 14 ---- corpo da lista (sem o ultimo elemento)
corpo :: [a] -> [a]
corpo [] = []
corpo x = init x

---- questao 15 ---- remove as repeticoes de elementos
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = if elem x (unique xs) 
                    then unique xs
                    else x:unique xs

---- questao 18 ---- reverso da lista [1,2,3] -> [3,2,1]
reverso :: [Int] -> [Int]
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]

---- questao 20 ---- intercalacao entre lista a e b
intercala :: [a] -> [a] -> [a]
intercala [] [] = []
intercala [] [x] = [x]
intercala [x] [] = [x]
intercala (x:xs) (y:ys) = x:y:intercala xs ys

----- questao 23 ----- n e m e retorna uma lista com n termos a partir de m
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n x = [x+i | i <- [0..n-1]]

----- questao 25 ----- 
isSorted :: [Int] -> Bool
isSorted [x] = True
isSorted (x:xs) = if x > head xs then False else isSorted xs

----- questao 32 ----- receber uma string e dizer se eh palindromo
isPalind :: String -> Bool
isPalind (x:xs)
 | (x:xs) == reversoS (x:xs) = True
 | otherwise = False

reversoS :: String -> String
reversoS [] = []
reversoS (x:xs) = (reversoS xs) ++ [x]