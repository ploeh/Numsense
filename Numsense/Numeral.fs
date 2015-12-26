module Ploeh.Numsense.Numeral

let toEnglish x = string x

let ofEnglish x = 
    match x with
    | "zero" -> 0
    | _ -> System.Int32.Parse x

