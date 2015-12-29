module Ploeh.Numsense.Numeral

open Ploeh.Numsense.InternalDsl

let toDanish =        Danish.toDanishImp 1
let tryParseDanish =  Danish.tryParseDanishImp

let toEnglish =       English.toEnglishImp
let tryParseEnglish = English.tryParseEnglishImp