module Ploeh.Numsense.Numeral

let toEnglish x = string x

let ofEnglish x = 
    match x with
    | "zero"  -> 0
    | "one"   -> 1
    | "two"   -> 2
    | "three" -> 3
    | "four"  -> 4
    | "five"  -> 5
    | "six"   -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine"  -> 9
    | _ -> System.Int32.Parse x

