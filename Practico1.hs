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

paratodo ::  [a] -> (a -> Bool) -> Bool
paratodo [] = True
paratodo (x:xs) t = t x && paratodo xs t


{- existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
predicado t :: a -> Bool, determina si alg ́un elemento de xs satisface el predicado
t. -}

existe :: [a] -> (a -> Bool) -> Bool
existe [] = False
existe (x:xs) t = t || existe xs t


{- sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
funcion t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
suma de los valores que resultan de la aplicacion de t a los elementos de xs. -}


sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t


{- productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
y una funci ́on t :: a -> Int, calcula el producto de los valores que resultan de la
aplicaci ́on de t a los elementos de xs. -}

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] = 1
productoria' (x:xs) t = t x * productoria' xs t 



