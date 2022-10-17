{-
Ejercicio 1
La funcion recibe como parametro una lista de elementos de cualquier valor. Lo que hace es juntar la cola
de la lista con el primer elemento de la lista.
-}
firstToEnd :: (Ord a) => [a] -> [a]
firstToEnd xs = tail xs ++ [head xs]

{-
Ejercicio 2
La funcion recibe como parametro una lista de elementos de cualquier valor. Lo que hace es retornar una 
lista con el valor minimo de la lista junto con el valor maximo de la lista
-}
minAndMax :: (Ord a) => [a] -> [a]
minAndMax xs = minimum xs : [maximum xs]

{-
Ejercicio 3
La funcion recibe como parametro una lista de elementos de valor entero. Primero checa si la lista 
introducida tiene o no valores o solo tiene uno, despues de corroborar ahora procede a realizar una
lista con los valores de la cola de la lista siempre y cuando dichos valores sean menores al valor 
del primer elemento de la lista.
-}
minorsFirstElement :: (Integral a) => [a] -> [a]
minorsFirstElement [] = error "Lista vacia"
minorsFirstElement [a] = error "Solo hay un elemento"
minorsFirstElement xs = [x | x<- tail xs , x < head xs]
{-
Ejercicio 4
La funcion recibe como parametro una lista de elementos de valor entero. Primero checa si la lista 
introducida tiene o no valores o solo tiene uno, despues de corroborar ahora procede a realizar una
lista con los valores de la cola de la lista siempre y cuando dichos valores sean mayores o iguales
al valor del primer elemento de la lista.
-}
greaterOrEqualFirstElement :: Integral a => [a] -> [a]
greaterOrEqualFirstElement [] = error "Lista vacia"
greaterOrEqualFirstElement [a] = error "Solo hay un elemento"
greaterOrEqualFirstElement xs = [x | x <- tail xs , x >=head xs]
{-
Ejercicio 5
La funcion recibe como parametro una lista de elementos de valor entero. Primero checa si la lista 
introducida tiene o no valores o solo tiene uno, despues de corroborar ahora procede a realizar una
lista con los valores de la cola de la cola de la lista siempre y cuando el valor sea la suma del 
primer elemento de la lista mas el primer elemento de la cola de la lista.
-}
minorsToSumFirstAndSecondElement :: Integral a => [a] -> [a]
minorsToSumFirstAndSecondElement [] = error "Lista vacia"
minorsToSumFirstAndSecondElement [a] = error "Solo hay un elemento"
minorsToSumFirstAndSecondElement xs= [x | x <- tail (tail xs) , x <head xs + head(tail xs)]
{-
Ejercicio 6
La funcion recibe como parametros una lista de duplas. Regresa una lista formada por la suma 
de los valores de las duplas. La funcion es recursiva y se detiene hasta que se ingrese una 
lista vacia.
-}
listSumDuplaToList :: Integral a => [(a,a)] -> [a]
listSumDuplaToList [] = []
listSumDuplaToList xs = fst(head xs) + snd (head xs) : listSumDuplaToList (tail xs)

{-
Ejercicio 7
En este caso se realiza una funcion para obtener cada valor de la tripleta (fst3 , snd3, thd3).
Despues la funcion general regresará una lista con los valores de la multiplicacion de los 
valores de cada tripleta y como este es recursivo se detendrá hasta que se ingrese una lista vacia.
-}
fst3 :: Num a => (a,a,a)->a
fst3 (x,y,z) = x
snd3 :: Num a => (a,a,a)->a
snd3 (x,y,z) = y
thd3 :: Num a => (a,a,a)->a
thd3 (x,y,z) = z

listMulTripletaToList :: (Integral a) => [(a,a,a)] -> [a]
listMulTripletaToList [] = []
listMulTripletaToList xs = fst3 (head xs) * snd3 (head xs) * thd3 (head xs) : listMulTripletaToList (tail xs)

{-
Ejercicio 8
La funcion recibe como parametros una lista de duplas. La funcion retorna una lista con 
la primer dupla de la lista pero posicionando el snd en el fst y el fst en el snd, despues
se le añaden el resto de las duplas con la misma funcion para invertir todas ellas. Como
el metodo es recursivo este se detendrá hasta que se pase una lista vacia.
-}
changeFstToSnd :: (Integral a) => [(a,a)] -> [(a,a)]
changeFstToSnd [] = []
changeFstToSnd xs = (snd (head xs),fst (head xs)) : changeFstToSnd (tail xs)

{-
Ejercicio 9
La funcion recibe como parametro una lista de duplas. La funcion retorna una dupla con el 
valor de la suma de todos los primeros elementos de cada dupla y con el valor de la suma
de todos los segundos elementos de cada dupla. Se usan dos recursiones para obtener la 
cola de la lista para cada valor de tupla. Si la cola de la lista llegase a tener solo un
valor, entonces solo regresa dicha dupla.
-}
sumVectors :: (Num a) => [(a,a)] -> (a,a)
sumVectors [x] = (fst x,snd x)
sumVectors xs = (fst (head xs) + fst (sumVectors (tail xs)) ,snd (head xs) + snd (sumVectors (tail xs)))

{-
Ejercicio 10
La funcion recibe como parametro un numero. Regresara una lista con todos los valores
que esten antes de dicho valor hasta el 1, siempre y cuando los valores se dividan 
con el mismo numero introducido y su residuo sea 0.
-}
dividers :: (Integral a) => a -> [a]
dividers n = [x | x <- [1..n], (mod n x) == 0]

{-
Ejercicio 11
Esta funcion recibe como parametros un numero. Regresara una lista con los numeros 
primos de 1 al numero ingresado. Para ello inicializamos a x como la lista a retornar
a x se le asigna la lista comprendida de 2 al numero ingresado, siempre y cuando sus
divisores sean el 1 y el mismo numero x. Se hace uso de la funcion dividers anteriormente
realizada.
-}
primeNumbers :: (Integral a) => a -> [a]
primeNumbers n = [x | x <- [2..n], dividers x == [1,x]]

{-
Ejercicio 12
Esta funcion no recibe parametros. Regresara una lista infinita con los numeros 
primos. Para ello inicializamos a x como la lista a retornara x se le asigna la lista 
comprendida de 2 al numero ingresado, siempre y cuando sus divisores sean el 1 y el 
mismo numero x. Se hace uso de la funcion dividers anteriormente realizada.
-}
infinitePrimeNumbers :: (Integral a) => [a]
infinitePrimeNumbers = [x | x <- [2..], dividers x == [1,x]]