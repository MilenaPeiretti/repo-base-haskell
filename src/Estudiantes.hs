module Estudiantes where
import PdePreludat

data Estudiante = Estudiante {
    saludMental :: Number,
    conocimiento :: Number
} deriving (Show)

primerizo :: Estudiante
primerizo = Estudiante {
    saludMental = 150,
    conocimiento = 0
}

recursante :: Estudiante
recursante = Estudiante {
    saludMental = 75,
    conocimiento = 70
}

data Tema = Tema {
    dificultad :: Number,
    aprendizaje :: Number
}

expresividad :: Tema
expresividad = Tema {
    dificultad = 30,
    aprendizaje = 100
}

declaratividad :: Tema
declaratividad = Tema {
    dificultad = 30,
    aprendizaje = 100
}

recursividad :: Tema 
recursividad = Tema {
    dificultad = 50,
    aprendizaje = 40
}

adquirirConocimiento :: Number ->Estudiante -> Estudiante
adquirirConocimiento cntd estudiante = estudiante {  conocimiento = conocimiento estudiante + cntd }

quemarseElBocho :: Number -> Estudiante -> Estudiante
quemarseElBocho cntd estudiante = estudiante { saludMental = saludMental estudiante - cntd }

estudiar :: Modificador -> Modificador -> Tema -> Estudiante -> Estudiante
estudiar  modAprender modEstres tema = estresarse modEstres tema . aprender modAprender tema

type Modificador = Number -> Number

estresarse :: Modificador -> Tema -> Estudiante -> Estudiante
estresarse modificador   = quemarseElBocho . modificador . dificultad 

aprender :: Modificador -> Tema -> Estudiante -> Estudiante
aprender modificador = adquirirConocimiento . modificador . aprendizaje

type MetodoDeEstudio = Tema -> Estudiante -> Estudiante

llevarlaAlDia :: MetodoDeEstudio
llevarlaAlDia   = estudiar id id

sinDormir :: MetodoDeEstudio
sinDormir  = estudiar ((*2) . (/3)) (*2) 

chatGpt :: MetodoDeEstudio
chatGpt  = estudiar (/3) (/2)

type SesionDeEstudio = [Tema]

hacerSesion :: SesionDeEstudio -> MetodoDeEstudio -> Estudiante -> Estudiante
hacerSesion temas metodo estudiante = foldl (\acum elem -> metodo elem acum) estudiante temas

todosLosTemas :: SesionDeEstudio
todosLosTemas = [expresividad, declaratividad, recursividad]

maratonDeExpresividad :: SesionDeEstudio
maratonDeExpresividad = repeat expresividad

maratonIntercalada :: SesionDeEstudio
maratonIntercalada = cycle [expresividad, declaratividad, recursividad]

repasoDeEmergencia :: SesionDeEstudio -> MetodoDeEstudio -> Estudiante -> Estudiante
repasoDeEmergencia   = hacerSesion . take 5