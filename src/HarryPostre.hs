module HarryPostre where
import PdePreludat

data Postre = Postre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre =  postre {
    peso = peso postre * 0.05,
    temperatura = temperatura postre + 1
}

immobulus :: Hechizo
immobulus postre = postre{
    temperatura = 0
}

wingardiumLeviosa :: String -> Hechizo
wingardiumLeviosa palabra postre = postre {
    sabores = sabores postre ++ [palabra],
    peso = peso postre * 0.1
}

diffindo :: Hechizo
diffindo postre = postre {
    peso = peso postre * 0.5
}

riddikulus :: String -> Hechizo
riddikulus sabor postre = postre {
    sabores = sabores postre ++ reverse[sabor]
}

avadaKedavra :: Hechizo
avadaKedavra postre = postre {
    sabores = [],
    temperatura = 0
}


estaListo :: Postre -> Bool
estaListo postre = (peso postre > 0) && (temperatura postre > 0) && (not (null (sabores postre)))

postresListos :: [Postre] -> Hechizo -> Bool
postresListos postres hechizo = all estaListo (map hechizo postres)

pesoPromedio :: [Postre] -> Number
pesoPromedio postres = sum (map peso (filter estaListo postres)) / length (filter estaListo postres)




data Mago = Mago {
    horrocruxes :: Number,
    hechizos :: [Hechizo]
} deriving (Show)


claseDeDefensa :: Mago -> Postre ->Hechizo -> Mago
claseDeDefensa mago postre hechizo = mago {
    hechizos = hechizos mago ++ [hechizo],
    horrocruxes = horrocruxes mago + sumoONo hechizo postre
}

sumoONo :: Hechizo -> Postre -> Number
sumoONo hechizo postre
    |hechizo postre == avadaKedavra postre = 1
    |otherwise = 0


mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago postre = foldl1 (\acum elem -> if length (sabores (elem postre)) > length (sabores (acum postre)) then elem else acum) (hechizos mago) 



listaInfinitaDePostres :: Postre -> [Postre]
listaInfinitaDePostres postre = postre : listaInfinitaDePostres postre

magoConInfinitosHechizos :: Mago
magoConInfinitosHechizos = Mago 0 (repeat incendio) 