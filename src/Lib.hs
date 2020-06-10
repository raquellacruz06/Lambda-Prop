module Lib where
import Text.Show.Functions

laVerdad = True


between n m x = elem x [n .. m]
type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = UnDepto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = UnaPersona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

--between cotaInferior cotaSuperior valor =
--valor <= cotaSuperior && valor >= cotaInferior
-- Criterio = (x-> x->Bool)

deptosDeEjemplo :: [Depto]
deptosDeEjemplo = [UnDepto 3 80 7500 "Palermo", UnDepto 1 45 3500 "Villa Urquiza", UnDepto 2 50 5000 "Palermo", UnDepto 1 45 5500 "Recoleta"]



  {-Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el 
resultado de evaluar esa función 
sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.-}

mayor :: Ord a => (b -> a) -> b -> b -> Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

menor :: Ord a => (b -> a) -> b -> b -> Bool
menor funcion valor1 valor2 = funcion valor1 < funcion valor2

--Mostrar un ejemplo de cómo se usaría una de estas funciones
-- para ordenar una lista de strings en base a su longitud usando ordenarSegun.

ordenarListaString :: [String] -> [String]
ordenarListaString lista = ordenarSegun (menor length) lista

{-Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si 
el departamento se encuentra en alguno de los barrios de la lista.-}

-- type requisito = Depto -> Bool
--busqueda = [requisitos]

ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barrios depto = elem (barrio depto) barrios

{-cumpleRango que a partir de una función y dos números, indique si 
el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.
-}
cumpleRango :: (Eq a, Enum a) => (Depto-> a) -> a -> a -> Requisito
cumpleRango funcion a b  = between a b . funcion 

--Definir la función cumpleBusqueda que se cumple si 
--todos los requisitos de una búsqueda se verifican para un departamento dado.

---busqueda = [requisitos], requisitos = Depto -> Bool
cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda listaRequisitos depto = all (cumpleRequisito depto) listaRequisitos

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto

{-Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, 
ordenado por mayor superficie, 
que cumplan con:
Encontrarse en Recoleta o Palermo 
Ser de 1 o 2 ambientes 
Alquilarse a menos de $6000 por mes
.-}
--Criterio = (x-> x->Bool)

busquedaEjemplo :: Busqueda
busquedaEjemplo = [ubicadoEn ["Recoleta", "Palermo"], cumpleRango ambientes 1 2, cumpleRango precio 0 6000]
-- cumpleBusqueda busquedaEjemplo UnDepto 3 80 7500 "Palermo" es FALSE

{-Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y 
una lista de departamentos retorne todos aquellos 
que cumplen con la búsqueda ordenados en base al criterio recibido.-}

buscar :: Busqueda -> (Depto -> Depto-> Bool) -> [Depto] -> [Depto]
buscar listaRequisitos criterio  = ordenarSegun criterio . departamentosQueCumplen listaRequisitos

departamentosQueCumplen :: Busqueda -> [Depto]-> [Depto]
departamentosQueCumplen listaRequisitos  = filter (cumpleBusqueda listaRequisitos) 

--Ejeplos: buscar busquedaEjemplo (mayor superficie)  [UnDepto 3 80 7500 "Palermo", UnDepto 1 45 3500 "Villa Urquiza"] 
--buscar busquedaEjemplo (mayor superficie)  [UnDepto 2 80 5000 "Palermo", UnDepto 1 45 3500 "Recoleta"] 
--Devuelve primero el de palermo
--buscar busquedaEjemplo (mayor superficie)  [UnDepto 2 80 2000 "Palermo", UnDepto 1 100 3500 "Recoleta"]
--Devuelve primero el de Recoleta

{-Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los 
mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.-}

mailsDePersonasInteresadas :: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas depto = map mail . personasConBusqueda depto  

personasConBusqueda :: Depto -> [Persona] -> [Persona]
personasConBusqueda depto personas = filter (tieneAlgunaBusqueda depto) personas

tieneAlgunaBusqueda :: Depto -> Persona ->  Bool
tieneAlgunaBusqueda depto  =  any  (flip cumpleBusqueda depto) . busquedas 
