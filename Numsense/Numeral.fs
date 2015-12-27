module Ploeh.Numsense.Numeral

let toEnglish x = string x

let ofEnglish x = 
    // Multiply the 'ty's
    let multys x =
        let ones = x % 10
        x + (10 * ones) - ones
    let multiplyHundreds x =
        let tens = x % 100
        x + (100 * tens) - tens
    let multiplyThousands x =
        let hundreds = x % 1000
        x + (1000 * hundreds) - hundreds
    let rec conv acc xs =        
        match xs with
        | []                                         -> acc
        | 'Z'::'E'::'R'::'O'::t                      -> conv  (0 + acc) t
        | 'O'::'N'::'E'::t                           -> conv  (1 + acc) t
        | 'T'::'W'::'O'::t                           -> conv  (2 + acc) t
        | 'T'::'H'::'R'::'E'::'E'::t                 -> conv  (3 + acc) t
        | 'F'::'O'::'U'::'R'::t                      -> conv  (4 + acc) t
        | 'F'::'I'::'V'::'E'::t                      -> conv  (5 + acc) t
        | 'S'::'I'::'X'::t                           -> conv  (6 + acc) t
        | 'S'::'E'::'V'::'E'::'N'::t                 -> conv  (7 + acc) t
        | 'E'::'I'::'G'::'H'::'T'::t                 -> conv  (8 + acc) t
        | 'N'::'I'::'N'::'E'::t                      -> conv  (9 + acc) t
        | 'T'::'E'::'N'::t                           -> conv (10 + acc) t
        | 'E'::'L'::'E'::'V'::'E'::'N'::t            -> conv (11 + acc) t
        | 'T'::'W'::'E'::'L'::'V'::'E'::t            -> conv (12 + acc) t
        | 'T'::'H'::'I'::'R'::'T'::'E'::'E'::'N'::t  -> conv (13 + acc) t
        | 'F'::'I'::'F'::'T'::'E'::'E'::'N'::t       -> conv (15 + acc) t
        | 'E'::'E'::'N'::t // matches 'een' in 'eighteen'
        | 'T'::'E'::'E'::'N'::t                      -> conv (10 + acc) t
        | 'T'::'W'::'E'::'N'::'T'::'Y'::t            -> conv (20 + acc) t
        | 'T'::'H'::'I'::'R'::'T'::'Y'::t            -> conv (30 + acc) t
        | 'F'::'O'::'R'::'T'::'Y'::t                 -> conv (40 + acc) t
        | 'F'::'I'::'F'::'T'::'Y'::t                 -> conv (50 + acc) t
        | 'Y'::t // matches 'y' in 'eighty'
        | 'T'::'Y'::t                                -> conv (multys acc) t
        | 'H'::'U'::'N'::'D'::'R'::'E'::'D'::t       ->
            conv (if acc = 0 then 100 else multiplyHundreds acc) t
        | 'T'::'H'::'O'::'U'::'S'::'A'::'N'::'D'::t  ->
            conv (if acc = 0 then 1000 else multiplyThousands acc) t
        | 'M'::'I'::'L'::'L'::'I'::'O'::'N'::t       -> conv (1000000 * acc) t
        | _ -> -1

    match System.Int32.TryParse x with
    | true, i -> i
    | _ -> conv 0 (x.Trim().ToUpper(System.Globalization.CultureInfo "en") |> Seq.toList)