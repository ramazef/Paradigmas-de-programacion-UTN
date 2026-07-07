module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


--data animal, Primer punto

data Visita = Visita{
  recuperacion :: Number,
  costoAtencion :: Number
} deriving (Show, Eq)

data Animal = Animal {
  nombre :: String,
  tipo :: String,
  peso :: Number,
  edad :: Number,
  enfermo :: Bool,
  visitas :: [Visita]
} deriving (Show, Eq)

--punto 1
laPasoMal :: Animal -> Bool
laPasoMal animal = any (\visita -> recuperacion visita > 30 ) (visitas animal)

nombreFalopa :: Animal -> Bool
nombreFalopa animal = ((=='i') . last . nombre) animal -- nombreFalopa animal = (last . nombre) animal == 'i'

engorde :: Number -> Animal -> Animal
engorde kilos animal | kilos >= 10 = animal {peso= (peso animal) + 5}
                     | otherwise = animal {peso= (peso animal) + (kilos/2)}

revisacion :: Number -> Number -> Animal -> Animal
revisacion diasReposo costo animal | enfermo animal == True = engorde 2 (animal {visitas = Visita { recuperacion = diasReposo, costoAtencion = costo } : visitas animal})
                                   | otherwise = animal

fiestaCumple :: Animal -> Animal
fiestaCumple animal = animal {edad = edad animal + 1, peso = peso animal -1}

chequeoPeso :: Number -> Animal -> Animal
chequeoPeso minimoPeso animal| peso animal < minimoPeso = animal {enfermo = True}
                              |otherwise = animal

type Actividad = Animal -> Animal 

data Proceso = Proceso{
  actividades :: [Actividad]
}
aplicarProceso :: Proceso -> Animal -> Animal
aplicarProceso proceso animal = foldr ($) animal (actividades proceso)

--nombreFalopa y laPasoMal no van porque no realizan nada sobre el animal
procesoEngorde = Proceso [engorde 5]
procesoRevisacion = Proceso [revisacion 3 100]
procesoFiestaCumple = Proceso [fiestaCumple]
procesoChequeoPeso = Proceso [chequeoPeso 10]

--o para probar todo de una
procesoCompleto :: Proceso
procesoCompleto = Proceso [engorde 5, revisacion 3 100, fiestaCumple, chequeoPeso 10]

vaca :: Animal
vaca = Animal {
  nombre = "Lola",
  tipo = "vaca",
  peso = 12,
  edad = 8,
  enfermo = False,
  visitas = []
}

--aplicarProceso procesoCompleto vaca

--4
mejoraONo :: [Actividad] -> Animal -> Bool
mejoraONo [actividad] animal = True

mejoraONo (actividad1:resto) animal = 
  pesaMas (animal) (actividad1 animal) 
  && 
  noSubio (animal) (actividad1 animal) 
  
  &&

  mejoraONo (resto) (animalAplicado)

  where 
    pesaMas animal1 animal2 = peso animal1 <= peso animal2
    noSubio animal1 animal2 = peso animal1 <= (peso animal2 +3)
    animalAplicado = actividad1 animal

primerosTresFalopa :: [Animal] -> [Animal]
primerosTresFalopa animales = take 3 (filter nombreFalopa animales)

--si, seria posible debido a que la lazy evaluation permite que si encuentra 3 
--ya se corta la evaluacion, da la respuesta y deja de evaluar