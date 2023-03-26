esCero :: Int -> Bool
esCero x = x == 0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool
esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          | otherwise = False

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x  = paratodo xs
                | otherwise = False

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

promedio :: [Int] -> Int
promedio xs = (sumatoria xs) `div` length xs

pertenece :: Int -> [Int] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                   | x /= y = pertenece x ys





{- paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el
predicado t. -}

paratodo' ::  [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo'(x:xs) t = t x && paratodo' xs t


{- existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
predicado t :: a -> Bool, determina si algun elemento de xs satisface el predicado
t. -}

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe'(x:xs) t = t x || existe' xs t  


{- sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
funcion t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
suma de los valores que resultan de la aplicacion de t a los elementos de xs. -}


sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t 


{- productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la
aplicaci ́on de t a los elementos de xs. -}

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t  = 1
productoria' (x:xs) t = t x * productoria' xs t 


{-6a-}

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

{-b-}
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> x `mod` n == 0)

{-c  sumaCuadrados :: Int -> Int, dado un n ́umero no negativo n, calcula la suma de
los primeros n cuadrados, es decir 〈∑ i : 0 ≤ i < n : i2 〉.
Ayuda: En Haskell se puede escribir la lista que contiene el rango de n ́umeros entre n
y m como [n..m].  -}

{- d  d) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursi ́on?-}

{-e-}
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (filter even xs) id


{-La función map toma una función y una lista como argumentos, devuelve una lista que aplica  a cada elemento de la lista-}
{- map ::  (a -> b) -> [a] -> [b]-}

{-La funcion filter toma  una funcion que devuelve un valor boleanoy una lista como argumentos devuelve una lista  que tiene los elementos que vuelve a True la funcion. -}
 {-filter :: (a -> Bool) -> [a] -> [a]-}

 {-map succ [1, -4, 6, 2, -8]  aplica la función succ a cada elemento de la lista, agrega 1 a cada elemento,devuelvela  lista [2, -3, 7, 3, -7] -
 filter esPositivo [1, -4, 6, 2, -8] aplica la funcion esPositivo a cada elemento de la lista, devuelve True solo si es el numero positivo y False para el neggativo,  devuelve la lista [2,3,7]-}
 
 {-8-}

duplicar :: [Int] -> [Int]
duplicar [] = []
duplicar (x:xs) = x*2 : duplicar xs

duplicar' :: [Int] -> [Int]
duplicar' xs = map (*2) xs

{-9 -}
paresRecursivo :: [Int] -> [Int]
paresRecursivo [] = []
paresRecursivo (x:xs)
  | even x    = x : paresRecursivo xs
  | otherwise = paresRecursivo xs

paresFilter :: [Int] -> [Int]
paresFilter xs = filter even xs


{-10-}
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA x [] = []
primIgualesA x (y:ys)
  | x == y    = y : primIgualesA x ys
  | otherwise = []

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' x = takeWhile (==x)

{-11-}

primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:xs) = x : takeWhile (== x) xs

