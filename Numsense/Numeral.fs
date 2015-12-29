module Ploeh.Numsense.Numeral

open Ploeh.Numsense.InternalDsl

let toDanish x = Danish.toDanishImp 1 x

let tryParseDanish x = Danish.tryParseDanishImp x

let toEnglish x = English.toEnglishImp x

let tryParseEnglish x = English.tryParseEnglishImp x