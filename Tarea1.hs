-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Luis Daniel Silvera Guzmán
--  Carnet: 12-11287
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado): 
      Se usa Qwen, Gemini y Deepseek para la explicación de sintáxis del lenguaje Haskell, verificación de funcionamiento de código, conceptos, instrucciones y estructuras más pequeñas para la construcción de las soluciones de los problemas.
      No se ha hecho uso de agentes de programación.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo x
    | onlyAlphaNum x == onlyAlphaNum (reverseString [] x) = True
    | otherwise = False

onlyAlphaNum :: String -> String
onlyAlphaNum [] = []
onlyAlphaNum (x:xs)
    | isAlphaNum x = toLower(x) : onlyAlphaNum xs
    | otherwise = onlyAlphaNum xs

reverseString :: [a] -> [a] -> [a]
reverseString tempList [] = tempList
reverseString tempList (x:xs) = reverseString (x:tempList) xs

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing 
mayorYMenor (x:xs) = Just (auxMYM (x, x) xs)

auxMYM :: Ord a => (a, a) -> [a] -> (a, a)
auxMYM (a, b) [] = (a, b)
auxMYM (a, b) (x:xs)
    | x > a = auxMYM (x, b) xs
    | x < b = auxMYM (a, x) xs
    | otherwise = auxMYM (a, b) xs

Porque solo puede tomar un valor mayor o menor para la tupla (a, b) si haya una en la lista que sea mayor o menor
que la que ya tiene del recorrido anterior.

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [c1, c2, ':', c4, c5]
    | not (isDigit c1  && isDigit c2 && isDigit c4 && isDigit c5) = Left "invalid format"
    | h > 23 = Left "hora fuera de rango"
    | m > 59 = Left "minutos fuera de rango"
    | otherwise = Right (h, m)
leerHora _ = Left "invalid format
    where 
    h = digitToInt (c1 * 10) + digitToInt c2
    m = digitToInt (c4 * 10) + digitToInt c5

duracion :: String -> String -> Either String Int
duracion = undefined

 s = undefined
-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
