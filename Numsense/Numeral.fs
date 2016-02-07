[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ploeh.Numsense.Numeral

let toDanish        = Danish.toDanishImp 1
let tryParseDanish  = Danish.tryParseDanishImp

let toEnglish       = English.toEnglishImp
let tryParseEnglish = English.tryParseEnglishImp

let toFarsi         = Farsi.toFarsiImp
let tryParseFarsi   = Farsi.tryParseFarsiImp

let toGerman =       German.toGermanImp
let tryParseGerman = German.tryParseGermanImp

let toDutch =         Dutch.toDutchImp
let tryParseDutch =   Dutch.tryParseDutchImp

let toRussian =       Russian.toRussianImp Russian.Masculine
let tryParseRussian = Russian.tryParseRussianImp

let toPolish        = Polish.toPolishImp
let tryParsePolish  = Polish.tryParsePolishImp

let toCatalan       = Catalan.toCatalanImp
let tryParseCatalan = Catalan.tryParseCatalanImp
