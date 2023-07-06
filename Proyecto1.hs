--Samsa Ahuerma.

---Ejercicio 1

esCero :: Int -> Bool
esCero x
   |(x  ==  0) = True
   |otherwise  =   False 



esPositivo :: Int -> Bool
esPositivo x 
   | (x > 0) = True  
   | otherwise = False



esVocal :: Char -> Bool
esVocal x 
   |x == 'a' = True
   |x == 'e' = True
   |x == 'i' = True
   |x == 'o' = True
   |x == 'u' = True
   |otherwise = False

---Ejercicio 2

paraTodo :: [Bool] -> Bool
paraTodo (x:xs) 
   | (x == True) = True
   | otherwise = False
 

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs


productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)


factorial :: Int -> Int
factorial 0 = 1
factorial x = x * (factorial (x-1))

promedio :: [Int] -> Int
promedio (x:xs) = div (sumatoria (x:xs)) (length(x:xs))

---Ejercicio 3

pertenece :: Int -> [Int] -> Bool
pertenece y [] = False
pertenece y (x:xs) 
   | y == x = True
   | otherwise = False

---Ejercicio 4

paratodo2 :: [a] -> (a -> Bool) -> Bool
paratodo2 [] t = True 
paratodo2 (x:xs) t = (t x) && (paratodo2 xs t)



existe' :: [a] -> (a -> Bool) -> Bool
existe' [] y = True 
existe' (x:xs) y = (y x) || (existe' xs y)



sumatoria' :: [a] -> (a -> Int) -> Int 
sumatoria' [] y = 0
sumatoria' (x:xs) y = (y x) + (sumatoria' xs y)



productoria' :: [a] -> (a -> Int) -> Int
productoria' [] y = 1
productoria' (x:xs) y = (y x) * (productoria' xs y) 

---Ejercicio 5

paratodo3 ::[Bool] -> Bool
paratodo3 x = paratodo2 x id 

---Ejercicio 6

todosPares :: [Int] -> Bool
todosPares xs = (paratodo2 xs even)


hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo y xs = existe' xs (\x -> mod x y == 0)



sumaCuadrados :: Int -> Int
sumaCuadrados m = sumatoria' [0..m] (\x-> x*x)


factorial2 :: Int -> Int
factorial2 m = productoria' [1..m] id


multiplicapares :: [Int] -> Int
multiplicapares xs = productoria (filter even xs)

---Ejercicio 7

---Lo que hace la función map es agarrar una función y una lista y aplicar la funcion a cada elemento de la lista devolviendo la una la lista con la función aplicada

---La función filter toma un predicado y una lista y devuelve los elementos de la lista que cumplan el predicado

---al aplicar map succ en la lista [1, -4, 6, 2, -8] me devuelve la lista [2, -3,  7, 3, -8]

---Al aplicar la funcion filter esPositivo en la lista me devuelve la lista [1, 6, 2]

---Ejercicio 8

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = 2*x : duplica xs



duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs

---Ejercicio 9

todospares1 :: [Int] -> [Int]
todospares1 [] = []
todospares1 (x:xs)
   | (\y -> mod y 2 == 0) x = x :(todospares1 xs)
   | otherwise = todospares1 xs


todospar :: [Int] -> [Int]
todospar xs = filter even xs



multiplicapares' :: [Int] -> Int
multiplicapares' xs = productoria (todospar xs)

---Ejercicio 10

primIgualesA :: (Eq a) => a -> [a] -> [a]
primIgualesA y [] = []
primIgualesA y (x:xs)
   | y == x = x : primIgualesA y xs
   | otherwise = []


primIgualesA' :: (Eq a) => a -> [a] -> [a]
primIgualesA' y xs = takeWhile (==y) xs  

---Ejercicio 11

primIguales :: (Eq a) => [a] -> [a]
primIguales [] = []
primIguales (x:xs)
   | x == head xs = x:(primIguales xs)
   | otherwise = x:[]


primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualesA (head xs) xs   

{-
12. (*) Para cada uno de los siguientes patrones, decidı si estan bien tipados, y en tal caso da los
tipos de cada subexpresion. En caso de estar bien tipado, ¿el patron cubre todos los casos
de definicion?

a)
f :: (a,b) -> ...
f x = ...
está bien tipado, x puede ser una dupla y la "imagen" de la funcion puede ser cualquier cosa. Cubre todos los casos

b)
f :: (a,b) -> ...
f (x,y) = ...
está bien tipado, el patron cubre cualquiera de los casos, sería como una versión extendida para
obtener de manera directa (a traves de x e y) el primer y segundo elemento de la dupla

c)
f :: [(a,b)] -> ...
f (a,b) = ...
no está bien tipado, ya que f recibe una lista de duplas, y en la definicion f está recibiendo solo
una dupla

d)
f :: [(a,b)] -> ...
f (x:xs) = ...
Está bien tipado, pide una lista de duplas, pero no cubre todos los casos, ya que al poner (x:xs) estamos
pidiendo que tenga al menos un elemento.

e)
f :: [(a,b)] -> ...
f ((x,y) : ((a,b) : xs)) = ...
Está bien tipado, pero como el item anteriro no cobre todos los casos, se tratá de una lista de duplas,
pero está pidiendo que tenga al menos 2 elementos.

f)
f :: [(Int, a)] -> ...
f [(0,a)] = ...
Está bien tipado, pero no cubre todos los casos. Pide una lista de duplas, en la que el primer elemento
debe ser un numero, y el segundo cualquier otra cosa, pero la función está definida para la lista de UN
elemento el cual será una dupla, cuyo primer elemento será '0' y el segundo cualquier otra cosa. Por lo
tanto no cubre todos los casos.

g)
f:: [(Int, a)] -> ...
f ((x,1):xs) = ...
Está bien tipado, pero no cubre todos los casos. Pide una lista de duplas, en la que el primer elemento
deber ser un numero, y el segundo cualquier cosa. Pero la función, primero que nada, está definida para
las listas con almenos UN elemento, no cubre el caso de una lista vacía. Y segundo, el segundo elemento
de la dupla está diciendo que sea un 1, cuando en realidda podría ser cualquier otra cosa.

h)
f:: [(Int, a)]
f ((1,x) : xs) = ...
Está bien tipado, pero no cubre todos los casos. Pide una lista de duplas, en la que el primer elemento
deber ser un numero, y el segundo cualquier cosa. Pero la función, primero que nada está definida para
las listas con almenos UN elemento, no cubre el caso de una lista vacía. Y segundo, está definida para
duplas en las que el primer elemento de cada dupla será un 1, cuando podría ser cualquier otro número.

i)
f:: (Int -> Int) -> Int -> ...
f a b = ...
Está bien tipado y cubre todos los casos. 'a' Sería una función que recibe un número y devuelve otro. 
Mientras que 'b' sería un número cualquiera

j)
f:: (Int -> Int) -> Int -> ...
f a 3 = ...
Está bien tipado, pero no cubre todos los casos. 'a' Sería una función que recibe un número y devuelve otro.
mientras que 'b' sería el 3, pero 'b' en realidad debería poder ser cualquier numero INT, esta funcion nada más
cubre los casos en los que 'b' = 3

k)
f:: (Int->Int) -> Int -> ...
f 0 1 2 = ...
Está mal tipado, la función está definido para dos argumentos, el primero una función y el segundo un numero.
Pero la función está siendo aplicada para 3 argumentos. los cuales serían 3 números.

l)
f :: a -> (a-> a) -> ...
f a g = ...
Está bien tipado y cubre todos los casos. 'a' Sería un elemento arbitario. 
Mientras que 'g' sería una función que recibe un argumento del mismo tipo que el elemento del primer argumento.


13. (*) Para las siguientes declaraciones de funciones, da al menos una defincion cuando sea
posible (que no sea la expresion undefined). ¿Podes dar alguna otra definicion alternativa
a la que diste en cada caso?

a) f:: (a,b) -> b
f (x,y) = y

f (x,y) = snd(x,y)

b) f :: (a,b) -> c
no se puede definir, no hay forma de definir un tipo 'c' a partir de una tupla de tipo (a,b)

c) f :: a -> b
De la misma forma que el anterior, no hay forma de que haskell pueda inferir en que clase de tipo deba ser 'b'. 
Pongamos un ejemplo: f 2 que me debería devolver? No hay forma de definir una función que en base a un argumento
me devuelva algo de otro tipo.

d)
f :: (a->b)-> a -> b
f func y = func y
ej:
f esVocal 1 := (esVocal 1) := false

e)
f :: (a->b)-> [a] -> [b]
No se puede definir ya que no podríamos inferir una lista de b ( [b] ) a partir de una funcion que va de a -> b y un segundo argumento
que es una lista de [a]. Ya que la funcion no se la podríamos aplicar al segundo argumento.

f)
f :: (a->b) -> a -> c
No se puede definir esta función, ya que no se podría obtener un tipo 'c' con los argumentos que tiene la función, ya que el primer argumento
va de a ->b y el segundo argumento es un a, si aplicamos la funcion al segundo argumento nos daría algo de tipo 'b' no de tipo 'c'

g)
f :: (a->b) -> (b->c) -> a -> c
f func1 func2 x = (func2 (func1 x))

-}
