module Guerreros where
import PdePreludat


type Entrenamiento = Guerrero -> Guerrero

data Raza = Saiyajin | Demonio | Humano | Namekusein deriving (Show, Eq)

data Guerrero = Guerrero {
    nombre :: String ,
    fuerza :: Number,
    razaGuerrero :: Raza ,
    transformaciones :: Number,
    entrenamientoFavorito :: Entrenamiento
} deriving (Show)

goku :: Guerrero
goku = Guerrero {
    nombre = "Goku",
    fuerza = 300,
    razaGuerrero = Saiyajin,
    transformaciones = 1,
    entrenamientoFavorito = entrenamientoIntenso
}

piccolo :: Guerrero
piccolo = Guerrero {
    nombre = "Piccolo",
    fuerza = 150,
    razaGuerrero = Namekusein,
    transformaciones = 2,
    entrenamientoFavorito = entrenamientoIntenso
}

yamcha :: Guerrero
yamcha = Guerrero {
    nombre = "Yamcha",
    fuerza = 50,
    razaGuerrero = Humano,
    transformaciones = 0,
    entrenamientoFavorito = entrenamientoMalo
}

freezer :: Guerrero
freezer = Guerrero {
    nombre = "Freezer",
    fuerza = 10000,
    razaGuerrero = Demonio,
    transformaciones = 1,
    entrenamientoFavorito = entrenamientoIntenso
}

dodoria :: Guerrero
dodoria = Guerrero {
    nombre = "Dodoria",
    fuerza = 10000,
    razaGuerrero = Demonio,
    transformaciones = 1,
    entrenamientoFavorito = entrenamientoVago
}


semillaDeErmitanio :: Guerrero -> Guerrero
semillaDeErmitanio guerrero =  guerrero {nombre = "Super" ++ (nombre guerrero), fuerza = (fuerza guerrero) + 4000}


transformacion :: Guerrero -> Guerrero
transformacion guerrero     | transformaciones guerrero < 3 = guerrero {transformaciones = transformaciones guerrero + 1, fuerza = (aumentoSegunRaza (razaGuerrero guerrero) + fuerza guerrero)}
                            | otherwise = guerrero

aumentoSegunRaza :: Raza -> Number
aumentoSegunRaza Saiyajin = 2000
aumentoSegunRaza Demonio = 5000
aumentoSegunRaza _ = 100

enfrentamiento :: Guerrero -> Guerrero -> Guerrero
enfrentamiento guerrero1 guerrero2 = guerreroMasPoderoso ( transformacion guerrero1) (transformacion guerrero2)

guerreroMasPoderoso :: Guerrero -> Guerrero -> Guerrero
guerreroMasPoderoso guerrero1 guerrero2     | fuerza guerrero1 < fuerza guerrero2 = guerrero2
                                            | otherwise = guerrero1

type Equipo = [Guerrero]

guerrerosZ :: Equipo
guerrerosZ = [goku, yamcha, piccolo]

fuerzasDeFreezer :: Equipo
fuerzasDeFreezer = [freezer, dodoria]

agregarMiembro :: Guerrero -> Equipo -> Equipo
agregarMiembro nuevo [] = [nuevo]
agregarMiembro nuevo equipo     | fuerza nuevo >= (fuerza (head equipo) /2 )= equipo ++ [nuevo]
                                | otherwise = equipo

equipoPreparado :: Equipo -> Bool
equipoPreparado equipo = all esFuerte equipo

esFuerte :: Guerrero -> Bool
esFuerte guerrero = fuerza guerrero > 1000

tienePesoMuerto :: Equipo -> Bool
tienePesoMuerto equipo = any esDebil equipo

esDebil :: Guerrero -> Bool
esDebil guerrero = fuerza guerrero < 100

equipoTransformado :: Equipo -> Equipo
equipoTransformado equipo = map transformacion equipo

type Humanos = [Guerrero]

esNativo :: Equipo -> Humanos
esNativo equipo = filter esHumano equipo

esHumano :: Guerrero -> Bool
esHumano guerrero = razaGuerrero guerrero == Humano

type Condicion = Equipo -> Bool 

data Torneo = Torneo {
    host :: Guerrero,
    participantes :: [Equipo],
    condicion :: Condicion
} deriving (Show)

budokaiTenkaichi :: Torneo
budokaiTenkaichi = Torneo {
    host = yamcha,
    participantes = [[goku, piccolo], [freezer, dodoria],[yamcha]],
    condicion = condicionBudokai
}

torneoDelPoder :: Torneo
torneoDelPoder = Torneo {
    host = piccolo,
    participantes = [[goku, freezer], [yamcha,piccolo]],
    condicion = condicionTorneoDelPoder
}

torneoLibre :: Torneo
torneoLibre = Torneo {
    host = freezer,
    participantes = [guerrerosZ, fuerzasDeFreezer],
    condicion = condicionLibre
}

nivelDeCompetitividad :: Torneo -> Number
nivelDeCompetitividad torneo = sum (map potencialDeEquipo (participantes torneo))

potencialDeEquipo :: Equipo -> Number
potencialDeEquipo equipo = sum (map fuerza equipo)

--conflictoDeIntereses :: Torneo -> Bool
--conflictoDeIntereses torneo = any (elem (host torneo)) (participantes torneo)

condicionBudokai :: Condicion
condicionBudokai equipo = length equipo == 2

condicionTorneoDelPoder :: Condicion
condicionTorneoDelPoder equipo = all fuerzaMenorADiezMil equipo

condicionLibre :: Condicion
condicionLibre equipo = True

fuerzaMenorADiezMil :: Guerrero -> Bool
fuerzaMenorADiezMil guerrero = fuerza guerrero < 10000

algunEquipoInvalido :: Torneo -> [Equipo]
algunEquipoInvalido torneo = filter (not . condicion torneo) (participantes torneo)


descalificarEquiposInvalidos :: Torneo -> Torneo
descalificarEquiposInvalidos torneo = torneo {participantes = filter (condicion torneo) (participantes torneo)}


entrenamientoIntenso :: Entrenamiento
entrenamientoIntenso guerrero =  guerrero {fuerza = fuerza guerrero + 100}

entrenamientoVago :: Entrenamiento
entrenamientoVago guerrero = guerrero 

entrenamientoMalo :: Entrenamiento
entrenamientoMalo guerrero = guerrero {fuerza = fuerza guerrero - 50}

equipoEntrenado :: Equipo -> Entrenamiento -> Equipo
equipoEntrenado equipo entrenamiento = map entrenamiento equipo

entrenamientoPersonalizado :: Guerrero -> Guerrero
entrenamientoPersonalizado guerrero = (entrenamientoFavorito guerrero) guerrero