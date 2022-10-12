import Data.List (elemIndex)
{-
Estas dos funciones las ocupo para poder convertir un numero en un arreglo/lista con los digitos que
componen al numero introducido
-}
convertNumToArray :: Integral a => a ->[a]
convertNumToArray num 
    | num<10 = [num]
    |otherwise = (rem num 10) : convertNumToArray(div num 10)

convertNumToArray' :: Integral a => a->[a]
convertNumToArray' num = reverse (convertNumToArray num)
{-
Ejercicio 1
La funcion recibe tres valores como parametros para despues sumarlos y al resultado dividirlo entre
el mismo numero de parametros
-}
average3Numbers :: Fractional a => a -> a -> a -> a
average3Numbers n1 n2 n3 = (n1+n2+n3)/3
{-
Ejercicio 2
La funcion recibe un numero como parametro, despues se utiliza la funcion de haskell llamada 'rem' 
esta regresa el residuo de la division. En este caso se divide sobre 10 para obtener como residuo
al ultimo digito del numero.
-}
isLastDigit3 :: Integral a => a -> Bool
isLastDigit3 num = (rem num 10) == 3
{-
Ejercicio 3
la funcion recibe un numero como parametro y solamente corrobora que el numero se encuentre entre 
el rango en el que se considere que son 3 digitos
-}
has3digits :: Integral a => a -> Bool
has3digits num = num>99 && num<1000;
{-
Ejercicio 4
La funcion recibe un numero como parametro. Esta solamente corrobora que el numero sea menor a 0
-}
isNegative :: Integral a => a -> Bool
isNegative num = num<0
{-
Ejercicio 5
La funcion recibe un numero como parametro. Se hace una comparacion para que se corrobore que sea
de dos digitos, si pasa esa condicion entonces suma el resultado de la division entera del numero
 con 10 con el resultado del residuo de la division con 10 con la funcion rem
-}
sum2Digits :: Integral a => a -> a
sum2Digits num
    | num <10 || num>99 = error "Necesito un numero de dos digitos"
    |otherwise = div num 10 + rem num 10

{-
Ejercicio 6
La funcion acepta como parametro un numero de dos digitos, arroja un error si no es de dos
digitos. La funcion devuelve un True si ambos digitos del numero son pares, en caso contrario
devuelve un False
-}
even2Digit:: Int->Bool
even2Digit num 
    | (num>9 && num<100) = even (mod num 10) && even(mod (div num 10) 10) 
    | otherwise = error "El numero no es de dos digitos "

{-
Ejercicio 7
La funcion recibe como parametro un numero. Despues determina si dicho numero pertenece a los
primeros 8 numeros primos , regresa un True si es asi de lo contrario regresa False
-}
isPrimeNumber :: Integer -> Bool
isPrimeNumber num = (num == 2 || num==3|| num == 5 || num==7 ||num == 11 || num==13|| num == 17 || num==19)

{-
Ejercicio 8
La funcion recibe un numero como parametro. Realiza las comparaciones de que si el numero
ingresado es par y si es de los primeros 8 numeros primos, en dicho caso devolvera un True
en caso contrario devolvera un False
-}
isEvenAndPrimeNumber :: Int ->Bool
isEvenAndPrimeNumber num =(num == 2 || num==3|| num == 5 || num==7 ||num == 11 || num==13|| num == 17 || num==19 ) && even num

{-
Ejercicio 9
La funcion recibe dos numeros como parametros. Calcula si el primer numero es multiplo del segundo
con la verificacion de su division de 0 
-}
isMultiple :: (Integral a) => a -> a -> Bool
isMultiple n1 n2
    |rem n1 n2 == 0 = True
    |otherwise = False

{-
Ejercicio 10
Funcion que acepta un numero como parametro. Esta funcion compara si los dos digitos son iguales,
ademas verifica que el numero ingresado sea de dos digitos, sino regresará un False directamente.
-}
isEqual2Digits :: (Integral a) => a -> Bool
isEqual2Digits num
    |(num > 9 && num < 100) && (div num 10 == mod num 10) = True
    |otherwise = False

{-
Ejercicio 11
La funcion tiene como parametros 3 numeros. Saca el maximo de los dos ultimos numeros para despues
sacar el maximo con dicho maximo y el primer numero ingresado
-}
higher :: (Integral a) => a -> a -> a -> a
higher n1 n2 n3 = max n1 (max n2 n3)

{-
Ejercicio 12
La funcion recibe dos numeros como parametros. Suma los dos numeros y luego determina si el
resultado es par con la funcion 'even'
-}
isEvenSum2Number :: (Integral a) => a -> a -> Bool
isEvenSum2Number n1 n2 = even (n1+n2)

{-
Ejercicio 13
Funcion que recibe dos numeros como parametros. La funcion suma los digitos de ambos numeros
siempre y cuando ambos numeros sean de dos digitos
-}
sum2Digit2Number :: (Integral a) => a -> a -> a
sum2Digit2Number n1 n2
    |n1 > 9 && n1 <100 && n2 > 9 && n2 <100 = div n1 10 + mod n1 10 + div n2 10 + mod n2 10
    |otherwise = error "Algún número no tiene 2 dígitos"

{-
Ejercicio 14
Funcion que recibe un numero como parametro. La funcion corrobora que el numero ingresado sea
de 3 digitos, si es asi suma los valores de los 3 digitos, sino entonces arroja un error
indicando que el numero ingresado no tiene 3 digitos
-}
sum3Digits :: (Integral a) => a -> a
sum3Digits num
    |num > 99 && num <1000 = (div num 100 + div (mod num 100) 10 + mod (mod num 100) 10)
    |otherwise = error "El número no tiene 3 dígitos"

{-
Ejercicio 15
La funcion acepta un numero como parametro, siempre y cuando este sea de 3 digitos. La funcion
obtiene los 3 digitos, despues arroja un True siempre y cuando dos de esos digitos son iguales
en caso contrario arrojará un False. La funcion arrojará un error si el numero ingresado es de
mas de 3 digitos.
-}
equal3Digits :: (Integral a) => a -> Bool
equal3Digits num 
    |(num > 99 && num <1000) && (cent == dece || cent == unid || dece == unid)= True
    |(num > 99 && num <1000) && (cent /= dece && cent /= unid && dece /= unid)= False
    |otherwise = error "El número no tiene 3 dígitos"
    where
        cent = div num 100
        dece = div (mod num 100) 10
        unid = mod (mod num 100) 10
{-
Ejercicio 16
La funcion recibe un numero, despues corrobora que ese numero tenga 3 digitos para despues sacar el maximo
valor de dicho numero convertido en lista con la funcion convertNumberToArray' que se encuentra al 
principio de este archivo. Si el numero no es de 3 digitos arrojara un mensaje de error que este no tiene 
los digitos optimos para la correcta funcion. 
-}
positionHigher3Digits :: Integer -> String
positionHigher3Digits num
 | num > 99 && num < 1000  = 
    let Just pos = elemIndex (maximum (convertNumToArray' num)) (convertNumToArray' num) 
    in "El mayor se encuentra en la pos " ++ show(pos)
 |otherwise = error "Este numero no tiene 3 digitos"

{-
Ejercicio 17
La funcion recibe una cadena de caracteres. Luego iguala si dicha cadena es igual con su inversa
con la funcion reverse
-}
palindrome :: Eq a => [a] -> Bool
palindrome cad = cad == reverse cad

{-
Ejercicio 18
La funcion recibe como parametros dos numeros. Esta funcion realiza la division como se hace
comunmente, sin embargo corrobora que el divisor, el numero n2, no sea 0
-}
safeDivision :: (Ord p, Floating p) => p -> p -> p
safeDivision n1 n2
    |n2 == 0 = error "No es posible dividir entre 0"
    |otherwise = n1 / n2

{-
Ejercicio 19
La funcion recibe como parametros dos valores booleanos. Realiza las comprobaciones logicas que
sigue la tabla de xor. Mientras sean distintos arroja True, en caso contrario arrojaria False
-}
xor :: Bool -> Bool -> Bool
xor bool1 bool2
    |bool1 /= bool2 = True
    |otherwise = False

{-
Ejercicio 20
La funcion recibe como parametros dos puntos en un plano cartesiano. La funcion representa la
formula de la distancia entre dos puntos. Resta los dos valores de x y los dos valores de y, 
despues suma los cuadrados de ambos resultados y a dicho resultado se le saca la raiz cuadrada 
con la funcion sqrt de haskell
-}
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
