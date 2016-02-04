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

let toTraditionalChinese =
    Chinese.toChineseImp Chinese.traditionalCharacterSet
let tryParseTraditionalChinese =
    Chinese.tryParseChineseImp Chinese.traditionalCharacterSet

let toTraditionalFinancialChinese =
    Chinese.toChineseImp Chinese.traditionalFinancialCharacterSet
let tryParseTraditionalFinancialChinese =
    Chinese.tryParseChineseImp Chinese.traditionalFinancialCharacterSet

let toSimplifiedChinese =
    Chinese.toChineseImp Chinese.simplifiedCharacterSet
let tryParseSimplifiedChinese =
    Chinese.tryParseChineseImp Chinese.simplifiedCharacterSet

let toSimplifiedFinancialChinese =
    Chinese.toChineseImp Chinese.simplifiedFinancialCharacterSet
let tryParseSimplifiedFinancialChinese =
    Chinese.tryParseChineseImp Chinese.simplifiedFinancialCharacterSet

let toCatalan       = Catalan.toCatalanImp
let tryParseCatalan = Catalan.tryParseCatalanImp
