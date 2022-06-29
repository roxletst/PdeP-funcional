module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Clase 1

aproboAlumno :: Number -> Bool
aproboAlumno nota = nota >= 6

maximaNota :: Number -> Number -> Number
maximaNota unaNota otraNota | unaNota > otraNota = unaNota
                            | otherwise = otraNota

f :: Number -> Number
f numero 
    | numero > 5                = numero
    | numero >= 0 && numero <=5 = numero -1
    | otherwise                 = 2 * numero + 3

-- Resolucion Kata 1

funcionLoca :: Number -> String -> Number
funcionLoca numero palabra
    | odd numero = numero
    | length palabra > numero = length palabra
    | otherwise = numero `mod` (length palabra)

-- Clase 3, parte 1

siguiente :: Number -> Number
siguiente num = num + 1

esPar :: Number -> Bool
esPar numb = mod numb 2 == 0

elSiguienteEsPar :: Number -> Bool
-- elSiguienteEsPar numero = (esPar . siguiente) numero
elSiguienteEsPar  = even . (1+) -- point-free gato

type Nombre = String
type Edad = Number
type Domicilio = String
type Person = (Nombre, Edad, Domicilio) -- un nuevo tipo de dato, luego agregue domicilio.

agus :: Person
agus = ("Agustin", 14, "Pepito Namuncura 444")

clara :: Person
clara = ("Clara", 27, "Mariano Moreno 4254")

edad :: Person -> Number
--edad = snd                                        -- opcion 1
edad (_, _edad, _) = _edad                          -- opcion 2, luego de agregar Domicilio, se agrega la tercera variable anónima por Pattern Matching

esMayor :: Edad -> Bool
esMayor edad = edad >= 18

mayorDeEdad :: Person -> Bool
--mayorDeEdad persona = (esMayor . edad) persona   -- mayorDeEdad = esMayor . edad   -- point-free
mayorDeEdad persona = ((>= 18) . edad) persona
--esMayorDeEdad :: Persona -> Bool
--esMayorDeEdad persona = (edad persona) >= 18

_nombre :: Person -> Nombre                        -- cambié definicion por _nombre, usado en data con Record Syntax
_nombre (nombre, _, _) = nombre

nombrePersonaEmpiezaConC :: Person -> Bool
nombrePersonaEmpiezaConC = (=='C') . head . _nombre

-- Clase 3, parte 2

cabeza (primerElemento: _) = primerElemento     -- definicion de head
cola (_: _cola) = _cola                         -- definicion de tail

losPrimerosDiez = take 10                       -- aplicacion parcial de take y point-free

type Nota = Number
type Alumno2 = (Nombre, [Nota])

-- data Persona = Persona Nombre [Nota]        -- sin Record Syntax

{-data Persona = Persona {                       -- con Record Syntax
    nombre :: Nombre,
    notas :: [Nota]
    } deriving (Show)                          -- Lo necesito para poder mostrar persona en consola


nombrePersona :: Persona -> Nombre
nombrePersona (Persona nombre _ )= nombre

santiago = Persona "Santi" [8,8,10]             -- sin Record Syntax

fede = Persona {                                -- con Record Syntax
    notas = [9,9,10],
    nombre = "Federico"
}
-}

-- Clase 4. Ejercicio integrador

data Parcial = Parcial {
    materia :: String,
    cantidadDePreguntas :: Number
} deriving (Show)

type CriterioEstudio = Parcial -> Bool

estudioso :: CriterioEstudio
estudioso _ = True                              -- no importa el parcial que reciba, le pongo var anonima

hijoDelRigor :: Number -> CriterioEstudio
hijoDelRigor preguntasLimite = (> preguntasLimite) . cantidadDePreguntas 

cabulero :: CriterioEstudio
cabulero = odd . length . materia

type Dia = Number
type Mes = Number
type Anio = Number
type Fecha = (Dia,Mes,Anio)

data Alumno = Alumno {
    nombre :: String,
    nacimiento :: Fecha,
    legajo :: Number,
    materias :: [String],
    criterioEstudio :: CriterioEstudio
} deriving (Show)

nico :: Alumno
nico = Alumno {
    nombre = "Nicolas",
    nacimiento = (7,9,1987),
    legajo = 111111,
    materias = ["PdeP", "Sintaxis", "AMII"],
    criterioEstudio = estudioso
} 

cambiarCriterioDeEstudio :: CriterioEstudio -> Alumno -> Alumno
cambiarCriterioDeEstudio nuevoCriterioEstudio alumno = alumno {
    criterioEstudio = nuevoCriterioEstudio
}

parcialPdep = Parcial {
    materia = "Paradigmas de Programacion",
    cantidadDePreguntas = 2
}

vaAEstudiar :: Parcial -> Alumno -> Bool
vaAEstudiar parcial alumno = (criterioEstudio alumno) parcial

-- Clase 5. Recursividad

factorial :: Number -> Number
{-factorial n                               -- Definicion por guardas
    | n == 0 = 1
    | n > 0 = n * factorial (n-1) -}

factorial 0 = 1                             -- Definicion por Pattern Matching
factorial n = n * factorial (n-1)

largoLista :: [a] -> Number
largoLista [] = 0
largoLista (_:cola) = 1 + largoLista cola

ultimoLista :: [a] -> a
ultimoLista [x] = x
ultimoLista (_:xs) = ultimoLista xs
-- ultimoLista [] = error "No se puede obtener el ultimo elemento de una lista vacia"

tomarElementos :: Number -> [a] -> [a]
tomarElementos numero [] | numero <=0 = []
tomarElementos 0 (_:_) = []
tomarElementos numero (x:xs) = x : tomarElementos (numero-1) xs 

elementoEsta :: Eq a => a -> [a] -> Bool
elementoEsta _ [] = False
elementoEsta elemento (x:xs) =  x == elemento || elementoEsta elemento xs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

transformarABinario :: Number -> [Number]
transformarABinario numero 
    | numero < 2 = [numero]
    | otherwise = transformarABinario (div numero 2) ++ [mod numero 2]

transformarABase :: Number -> Number -> [Number]
transformarABase base numero 
    | numero < base = [numero]
    | otherwise = transformarABase base (div numero base) ++ [mod numero base]

transformarABase' :: Number -> Number -> String             -- Otra forma, para mostrar el numero como String y no como lista de numeros. Uso de show
transformarABase' base numero 
    | numero < base = show numero
    | otherwise = transformarABase' base (div numero base) ++ (show. mod numero) base

sumatoria' :: Number -> Number
sumatoria' numero
    | numero <= 0 = 0
    | otherwise = sumatoria' (numero - 1) + numero

sumatoria :: Number -> Number -> Number
sumatoria numero total
    | numero <= 0 = total
    | otherwise = sumatoria (numero - 1) ( total + numero )

-- Lazy Evaluation

sumaAbsurda :: Number -> Number -> Number
sumaAbsurda x y = x + x

listaInfinita :: Number -> [Number]
listaInfinita numeroInicial = numeroInicial : listaInfinita (numeroInicial + 1)

divisiblesPor7 :: Number -> [Number]
divisiblesPor7 numero
 | mod numero 7 /= 0 = divisiblesPor7 (numero +1)
 | otherwise = numero : divisiblesPor7 (numero +1)

--cantidadDeDivisiblesPor7 :: Number -> Number -> [Number]
--cantidadDeDivisiblesPor7 numero total 

-- Clase 5. Orden Superior

data Cliente = Cliente {
    nombre' :: String,
    deuda :: Number,
    facturas :: [Number]
} deriving (Show)

debenMasDe10000 :: [Cliente] -> [String]
debenMasDe10000 [] = []
debenMasDe10000 (cliente:clientes)
    | ((<10000) .deuda) cliente = debenMasDe10000 clientes
    | otherwise = nombre' cliente : debenMasDe10000 clientes

clientesQueDeben :: Number -> [Cliente] -> [Cliente]
clientesQueDeben _ [] = []
clientesQueDeben plata (cliente:clientes)
    | ((> plata) . deuda) cliente = cliente: clientesQueDeben plata clientes
    | otherwise = clientesQueDeben plata clientes


misClientes = [
    Cliente "La Abuelita" 10000 [50000,12000],
    Cliente "La Sole" 1000 [4000,8000],
    Cliente "neuquen" 12000 [500000,1000]
 ]

clientesConPalindromo :: [Cliente] -> [String]
clientesConPalindromo [] = []
clientesConPalindromo (cliente: clientes)
 | (reverse . nombre') cliente /= nombre' cliente = clientesConPalindromo clientes
 | otherwise                                      = nombre' cliente : clientesConPalindromo clientes

clientesConFactura :: Number -> [Cliente] -> [Cliente]
clientesConFactura _ [] = []
clientesConFactura factura (cliente:clientes)
    | (elem factura . facturas) cliente = cliente: clientesConFactura factura clientes
    | otherwise = clientesConFactura factura clientes

-- usando orden superior

palindromo :: String -> Bool
palindromo palabra = reverse palabra == palabra

clientesPalindromo :: [Cliente] -> [Cliente]
clientesPalindromo clientes = filter (palindromo . nombre') clientes

cuantoMeDeben :: [Cliente] -> Number
cuantoMeDeben clientes = (sum . map deuda) clientes