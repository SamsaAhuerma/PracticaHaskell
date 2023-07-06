--Proyecto 2/Samsa Ahuerma
--Ejercicio 1
--a 
data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Eq, Ord, Show, Bounded, Enum)

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

--Ejercicio 20
--Esta hecho en la linea 4

--Ejercicio 3 
--a
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar  deriving (Eq,Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado  deriving (Eq,Show)

data Persona = Decane 
          | Docente Cargo
          | NoDocente Area 
          | Estudiante Carrera Ingreso 
          deriving (Eq,Show)

--b)¿Cual es el tipo del constructor Docente?
--Docente :: Cargo -> Persona toma un cargo y devuelve una persona

--c
cuantos_doc :: [Persona] -> Cargo -> Int   
cuantos_doc [] c = 0
cuantos_doc (x:xs) c| x==Docente c = 1 + (cuantos_doc xs c)
                    | otherwise = cuantos_doc xs c 

--d
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' (x:xs) c = length(filter(==Docente c)(x:xs))

--Ejercicio 4
--a
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just (x)

--Ejercicio 5
data Cola = VaciaC | Encolada Persona Cola 
          deriving (Eq,Show)
--a-1
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada x y) = Just y

atender' :: Cola -> Maybe Cola
atender' VaciaC = Nothing
atender' (Encolada _ VaciaC) = Just VaciaC
atender' (Encolada x (Encolada y z)) = if z==VaciaC then Just (Encolada x z) else atender' (Encolada y z)

--a-2
encolar :: Persona -> Cola -> Cola
encolar x y = Encolada x y

--a-3
busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC c = Nothing
busca (Encolada x (Encolada y cola)) c = if cola==VaciaC then (if y==Docente c then Just y else busca (Encolada x cola) c) else busca (atender'' (Encolada x (Encolada y cola))) c

atender'' :: Cola -> Cola
atender'' VaciaC = Nothing
atender'' (Encolada _ VaciaC) = Just VaciaC
atender'' (Encolada x (Encolada y z)) = if z==VaciaC then Just (Encolada x z) else atender' (Encolada y z)

--b) ¿A que otro tipo se parece Cola?
--Se parece a una lista.

--Ejercicio  6(Encolada x (Encolada y cola))
data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b ) deriving (Show)
--a
type GuiaTel = ListaAsoc String Int

--b-1
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b(la)) = 1 + la_long la

--b-2
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia Vacia = Vacia
la_concat z Vacia = z
la_concat Vacia z = z
la_concat (Nodo x y (z)) (Nodo a b (la)) = Nodo x y (la_concat z(Nodo a b(la)))

--b-3
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b (la)) = (a,b):(la_pares (la)) 

--b-4
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia c = Nothing
la_busca (Nodo a b (la)) c | a == c = Just b
                           |otherwise = (la_busca la c)

--b-5
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar x Vacia = Vacia
la_borrar x (Nodo a b (la)) | a == x = (la)
                            | otherwise = (la_borrar x (la))


--Ejercicio 7*
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving (Show)
--a) a_long :: Arbol a -> Int que dado un  ́arbol devuelve la cantidad de datos almacenados.
a_long :: Arbol a -> Int
a_long (Rama x a y) = 1 + a_long x + a_long y

--b) a_hojas :: Arbol a -> Int que dado un arbol devuelve la cantidad de hojas.
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama x a y) = (a_hojas x) + (a_hojas y)

--c) a_inc :: Num a => Arbol a -> Arbol a que dado un  ́arbol que contiene n ́umeros,los incrementa en uno.
a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc(Rama x a y) = Rama(a_inc x) (a + 1) (a_inc y)

--d) a_map :: (a -> b) -> Arbol a -> Arbol b que dada una función y un arbol, devuele el arbol con la misma estructura, que resulta de aplicar la función a cada uno de los elementos del arbol. Revisa la definción de la función anterior y reprogramala usando a_map.
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map fun Hoja = Hoja
a_map fun(Rama x a y) = Rama (a_map fun x) (fun a) (a_map fun y)




