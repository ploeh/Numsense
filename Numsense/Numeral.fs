[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ploeh.Numsense.Numeral

let toDanish        = Danish.toDanishImp 1
let tryParseDanish  = Danish.tryParseDanishImp

let toEnglish       = English.toEnglishImp
let tryParseEnglish = English.tryParseEnglishImp

let toFarsi         = Farsi.toFarsiImp
let tryParseFarsi   = Farsi.tryParseFarsiImp

let toPolish        = Polish.toPolishImp
let tryParsePolish  = Polish.tryParsePolishImp

let toDutch         = Dutch.toDutchImp
let tryParseDutch   = Dutch.tryParseDutchImp

let toRussian       = Russian.toRussianImp Russian.Masculine
let tryParseRussian = Russian.tryParseRussianImp

let toCatalan       = Catalan.toCatalanImp
let tryParseCatalan = Catalan.tryParseCatalanImp

let toSwedish =       Swedish.toSwedishImp
let tryParseSwedish = Swedish.tryParseSwedishImp