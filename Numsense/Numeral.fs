module Ploeh.Numsense.Numeral

let toEnglish x = string x

let ofEnglish x = 
    match System.Int32.TryParse x with
    | true, i -> i
    | _ -> 

    match x.Trim().ToUpper() with
    | "ZERO"      ->  0
    | "ONE"       ->  1
    | "TWO"       ->  2
    | "THREE"     ->  3
    | "FOUR"      ->  4
    | "FIVE"      ->  5
    | "SIX"       ->  6
    | "SEVEN"     ->  7
    | "EIGHT"     ->  8
    | "NINE"      ->  9
    | "TEN"       -> 10
    | "ELEVEN"    -> 11
    | "TWELVE"    -> 12
    | "THIRTEEN"  -> 13
    | "FOURTEEN"  -> 14
    | "FIFTEEN"   -> 15
    | "SIXTEEN"   -> 16
    | "SEVENTEEN" -> 17
    | "EIGHTEEN"  -> 18
    | "NINETEEN"  -> 19
    | _ -> -1

