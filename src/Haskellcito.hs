module Haskellcito where
import PdePreludat
import Data.Bits (Bits)

data Juego = Juego {
    nombre :: String,
    anio :: Number,
    modoCooperativo :: Bool,
    rating :: Number
} deriving (Show,Eq)

pdep :: Juego
pdep = Juego {
    nombre = "PdeP",
    anio = 2025,
    modoCooperativo = False,
    rating = 20
} 

juegoViejo :: Juego -> Bool
juegoViejo juego = anio juego < 2010

volverseCooperativo :: Juego -> Juego
volverseCooperativo juego   |modoCooperativo juego == True = juego
                            |juegoViejo juego = cambioJuegoViejo juego
                            |otherwise = cambioJuegoNuevo juego

cambioJuegoViejo :: Juego -> Juego
cambioJuegoViejo juego = juego {rating = rating juego + coeficienteDeDiversion juego, modoCooperativo = True }

cambioJuegoNuevo :: Juego -> Juego
cambioJuegoNuevo juego = juego {rating = rating juego + length (nombre juego) +35, modoCooperativo = True}

coeficienteDeDiversion :: Juego -> Number
coeficienteDeDiversion juego = length (nombre juego) + rating juego * 10

juegoCopado :: Juego -> Bool
juegoCopado juego = juegoViejo juego || modoCooperativo juego || nombreCopado (nombre juego)

nombreCopado :: String -> Bool
nombreCopado "PdeP" = True
nombreCopado nombre = last (nombre) == 'z' 

type Biblioteca = [Juego]

masJugado :: Biblioteca -> String
masJugado lista = nombre (head lista)

agregarABliblioteca :: Biblioteca -> Juego -> Biblioteca
agregarABliblioteca  [] juego = [juego]
agregarABliblioteca lista juego = lista ++ [secuela juego]

secuela :: Juego -> Juego
secuela juego = juego {anio = anio juego + 1, nombre = "2. La venganza " ++ nombre juego}


