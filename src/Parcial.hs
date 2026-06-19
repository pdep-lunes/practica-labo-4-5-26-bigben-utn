module Parcial where

import Text.Show.Functions()
import PdePreludat

type Actividad = (Ejercicio, Number)

obtenerEjercicio :: Actividad -> Ejercicio
obtenerEjercicio = fst

obtenerTiempo :: Actividad -> Number
obtenerTiempo = snd

data Guarderia = UnaGuarderia {
    nombreGuarderia :: String,
    actividades :: [Actividad]
} deriving Show

data Juguete = PeineDeGoma | Pelota | Mantita deriving Show

data Perro = UnPerro {
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempoEnGuarderia :: Number,
    energia :: Number
} deriving Show

cambiarEnergia :: (Number -> Number) -> Perro -> Perro
cambiarEnergia transformacion unPerro = unPerro { energia = transformacion $ energia unPerro }

cambiarJuguetes :: ([Juguete] -> [Juguete]) -> Perro -> Perro
cambiarJuguetes transformacion unPerro = unPerro { juguetesFavoritos = transformacion $ juguetesFavoritos unPerro }

-- Ejercicios Perrunos
type Ejercicio = Perro -> Perro

jugar :: Ejercicio
jugar unPerro = cambiarEnergia (restarConPiso 0 10) unPerro

ladrar :: Number -> Ejercicio
ladrar cantidadLadridos unPerro = cambiarEnergia (\unaEnergia -> unaEnergia + div cantidadLadridos 2) unPerro

regalar :: Juguete -> Ejercicio
regalar unJuguete unPerro = unPerro { juguetesFavoritos = unJuguete : juguetesFavoritos unPerro }

diaDeSpa :: Ejercicio
diaDeSpa unPerro
    | permaneceEnGuarderia 50 unPerro || esExtravagante unPerro = (regalar PeineDeGoma) . cambiarEnergia (\unaEnergia -> 100) $ unPerro
    | otherwise = unPerro

diaDeCampo :: Ejercicio
diaDeCampo unPerro = cambiarJuguetes tail . jugar $ unPerro

-- Funciones Auxiliares
razasExtravagantes :: [String]
razasExtravagantes = ["dalmata", "pomerania"]

esExtravagante :: Perro -> Bool
esExtravagante = (flip elem) razasExtravagantes . raza


permaneceEnGuarderia :: Number -> Perro -> Bool
permaneceEnGuarderia tiempoMinimo unPerro = tiempoEnGuarderia unPerro >= tiempoMinimo

restarConPiso :: Number -> Number -> Number -> Number
restarConPiso piso sustraendo minuendo = max piso $ minuendo - sustraendo

-- Modelado
zara :: Perro
zara = UnPerro {
    raza = "dalmata",
    juguetesFavoritos = [Pelota, Mantita],
    tiempoEnGuarderia = 1.5 * 60,
    energia = 80
}

doroty :: Perro
doroty = UnPerro {
    raza = "danes",
    juguetesFavoritos = [PeineDeGoma, Pelota, Pelota, Pelota, Pelota],
    tiempoEnGuarderia = 1000,
    energia = 15
}

guarderiaPDePerritos :: Guarderia
guarderiaPDePerritos = UnaGuarderia {
    nombreGuarderia = "GuarderiaPDePerritos",
    actividades = [(jugar, 30), (ladrar 18, 20), (regalar Pelota, 0), (diaDeSpa, 120), (diaDeCampo, 720)]
}

esAptoGuarderia :: Guarderia -> Perro -> Bool
esAptoGuarderia unaGuarderia unPerro = tiempoEnGuarderia unPerro > tiempoRutina unaGuarderia

esResponsable :: Perro -> Bool
esResponsable = (>3) . length . juguetesFavoritos . diaDeCampo

realizarRutina :: Guarderia -> Perro -> Perro
realizarRutina unaGuarderia unPerro
    | esAptoGuarderia unaGuarderia unPerro = realizarActividades (actividades unaGuarderia) unPerro
    | otherwise = unPerro

reportarCansados :: Guarderia -> [Perro] -> [Perro]
reportarCansados unaGuarderia listaPerros = filter (estaCansado . realizarRutina unaGuarderia) listaPerros

estaCansado :: Perro -> Bool
estaCansado (UnPerro _ _ _ energiaPerro) = energiaPerro < 5

-- Funciones Auxiliares para Parte 2
realizarActividades :: [Actividad] -> Perro -> Perro
realizarActividades [] unPerro = unPerro
realizarActividades (x:xs) unPerro = realizarActividades xs (obtenerEjercicio x unPerro)

tiempoRutina :: Guarderia -> Number
tiempoRutina unaGuarderia = sum . map (\unaActividad -> obtenerTiempo unaActividad) $ actividades unaGuarderia
