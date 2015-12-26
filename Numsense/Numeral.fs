module Ploeh.Numsense.Numeral

let toEnglish x = string x

let ofEnglish x = 
    match System.Int32.TryParse x with
    | true, i -> i
    | _ -> 

    match x.Trim().ToUpper() with
    | "ZERO"  -> 0
    | "ONE"   -> 1
    | "TWO"   -> 2
    | "THREE" -> 3
    | "FOUR"  -> 4
    | "FIVE"  -> 5
    | "SIX"   -> 6
    | "SEVEN" -> 7
    | "EIGHT" -> 8
    | "NINE"  -> 9
    | _ -> -1

