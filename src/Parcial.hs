module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

listaExtravagantes = ["dalmata", "pomerania"]

data Perro {
    raza :: String,
    juguetesFavoritos :: [String],
    tiempo :: Numero,
    energia :: Numero
} deriving Show

cambiarEnergia :: Numero -> Perro -> Numero
cambiarEnergia cantidadEnergia unPerro = energia unPerro + cantidadEnergia

jugar :: Perro -> Perro
jugar unPerro = unPerro { energia = max(0, cambiarEnergia (-10) unPerro) }

regalar :: String -> Perro -> Perro
regalar regalo unPerro = unPerro { juguetesFavoritos ++ [regalo] }

ladrar :: Numero -> Perro -> Perro
ladrar cantidadLadridos unPerro = setearEnergia ( (/2) $ energia unPerro ) unPerro

setearEnergia :: Numero -> Perro -> Perro
setearEnergia nuevaEnergia unPerro = unPerro { energia = nuevaEnergia }

masDe50Minutos :: Perro -> Bool
masDe50Minutos unPerro = tiempo unPerro >= 50

esExtravagante :: Perro -> Bool
esExtravagante unPerro = elem (raza unPerro) listaExtravagantes

esCandidatoASpa :: Perro -> Bool
esCandidatoASpa unPerro = masDe50Minutos unPerro && esExtravagante unPerro

diaDeSpa :: Perro -> Perro
diaDeSpa unPerro
    | esCandidatoASpa unPerro = setearEnergia 100 unPerro 
    | otherwise = unPerro

diaDeCampo :: Perro -> Perro
diaDeCampo unPerro = unPerro { juguetesFavoritos = drop 1 (juguetesFavoritos unPerro) }

horaYMedia = 60 * 1.5 -- en minutos

zara :: Perro
zara = {
    raza = "dalmata"
    juguetesFavoritos = ["pelota", "mantita"]
    tiempo = horaYMedia
    energia = 80
}

guarderiaPdePerritos = [    
    ("Jugar", 30)
    ("Ladrar 18", 20)
    ("Regalar pelota", 0)
    ("Día de spa", 120)
    ("Día de campo", 720)
]

aceptablePerroEnGuarderia :: [(String, Numero)] -> Perro -> Bool
aceptablePerroEnGuarderia rutinas unPerro = (sum . (map snd) $ guarderiaPdePerritos) >= tiempo unPerro

esPerroResponsable :: [(String, Numero)] -> Perro -> Bool
esPerroResponsable rutinas unPerro = (>3).length $ juguetesFavoritos (diaDeCampo unPerro)
