module internal Ploeh.Numsense.Italian

open Ploeh.Numsense.InternalDsl

let rec internal toItalianImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        match remainder with
        | 0 -> sprintf "%s" prefix
          // When a number is between 21 and 99 and the unit part starts with a
          // vowel (uno, otto), we must remove the last char from the tens part
          // For example: 21 -> "ventuno" (and not "ventiuno")
          //              38 -> "trentotto" (and not "trentaotto")
        | 1 
        | 8 when x > 20 && x < 100
            -> sprintf "%s%s" (prefix.[..prefix.Length - 2]) 
                              (toItalianImp remainder)
        | _ when x > 1000000 -> sprintf "%s %s" prefix (toItalianImp remainder)
        | _ -> sprintf "%s%s" prefix (toItalianImp remainder)

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toItalianImp (x / factor)) suffix
        simplify prefix factor x

    match x with
    |  x when x < 0 -> sprintf "meno %s" (toItalianImp -x)
    |  0 -> "zero"
    |  1 -> "uno"
    |  2 -> "due"
    |  3 -> "tre"
    |  4 -> "quattro"
    |  5 -> "cinque"
    |  6 -> "sei"
    |  7 -> "sette"
    |  8 -> "otto"
    |  9 -> "nove"
    | 10 -> "dieci"
    | 11 -> "undici"
    | 12 -> "dodici"
    | 13 -> "tredici"
    | 14 -> "quattordici"
    | 15 -> "quindici"
    | 16 -> "sedici"
    | 17 -> "diciassette"
    | 18 -> "diciotto"
    | 19 -> "diciannove"
    | Between 20 30 x -> simplify "venti" 10 x
    | Between 30 40 x -> simplify "trenta" 10 x
    | Between 40 50 x -> simplify "quaranta" 10 x
    | Between 50 60 x -> simplify "cinquanta" 10 x
    | Between 60 70 x -> simplify "sessanta" 10 x
    | Between 70 80 x -> simplify "settanta" 10 x
    | Between 80 90 x -> simplify "ottanta" 10 x
    | Between 90 100 x -> simplify "novanta" 10 x
    | Between 100 200 x -> simplify "cento" 100 x
    | Between 200 1000 x -> format "cento" 100 x
    | Between 1000 2000 x -> simplify "mille" 1000 x
    | Between 2000 1000000 x -> format "mila" 1000 x
    | Between 1000000 2000000 x -> simplify "un milione" 1000000 x
    | Between 2000000 1000000000 x -> format " milioni" 1000000 x
    | Between 1000000000 2000000000 x -> simplify "un miliardo" 1000000000 x
    | _ -> format " miliardi" 1000000000 x

let internal tryParseItalianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                          -> Some             acc
        | StartsWith " "            t
          // Matches final "-a" in: "trenta", "quaranta", "cinquanta", 
          // "sessanta", "settanta", "ottanta" and "novanta"
        | StartsWith "A"            t                            
          // Matches final "-e" in "milione"
        | StartsWith "E"            t
          // Matches final "-i" in "venti", "milioni" and "miliardi"
        | StartsWith "I"            t -> conv (           acc) t
        | StartsWith "ZERO"         t -> conv (      0  + acc) t
        | StartsWith "DUE"          t -> conv (      2  + acc) t
        | StartsWith "QUATTRO"      t -> conv (      4  + acc) t
        | StartsWith "CINQUE"       t -> conv (      5  + acc) t
        | StartsWith "SEI"          t -> conv (      6  + acc) t
        | StartsWith "SETTE"        t -> conv (      7  + acc) t
        | StartsWith "OTTO"         t -> conv (      8  + acc) t
        | StartsWith "NOVE"         t -> conv (      9  + acc) t
        | StartsWith "DIECI"        t -> conv (     10  + acc) t
        | StartsWith "UNDICI"       t -> conv (     11  + acc) t
          // Matches "un-" in "uno" and "un milione"
        | StartsWith "UN"           t -> conv (      1  + acc) t
        | StartsWith "DODICI"       t -> conv (     12  + acc) t
        | StartsWith "TREDICI"      t -> conv (     13  + acc) t
        | StartsWith "QUATTORDICI"  t -> conv (     14  + acc) t
        | StartsWith "QUINDICI"     t -> conv (     15  + acc) t
        | StartsWith "SEDICI"       t -> conv (     16  + acc) t
        | StartsWith "DICIASSETTE"  t -> conv (     17  + acc) t
        | StartsWith "DICIOTTO"     t -> conv (     18  + acc) t
        | StartsWith "DICIANNOVE"   t -> conv (     19  + acc) t
        | StartsWith "VENT"         t -> conv (     20  + acc) t
        | StartsWith "TRENT"        t -> conv (     30  + acc) t
        | StartsWith "TRE"          t -> conv (      3  + acc) t
        | StartsWith "QUARANT"      t -> conv (     40  + acc) t
        | StartsWith "CINQUANT"     t -> conv (     50  + acc) t
        | StartsWith "SESSANT"      t -> conv (     60  + acc) t
        | StartsWith "SETTANT"      t -> conv (     70  + acc) t
        | StartsWith "OTTANT"       t -> conv (     80  + acc) t
          // Matches final "-o" in "uno", "cento" and "miliardo"
        | StartsWith "O"            t -> conv (           acc) t
        | StartsWith "NOVANT"       t -> conv (     90  + acc) t
        | StartsWith "CENT"         t -> 
            conv (if acc = 0 then     100 else     100 %* acc) t
        | StartsWith "MILLE"        t -> conv (   1000  + acc) t
        | StartsWith "MILA"         t -> conv (   1000 %* acc) t
        | StartsWith "MILION"       t ->
            conv (if acc = 0 then 1000000 else 1000000 %* acc) t
        | StartsWith "MILIARD"      t ->
            conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "it")
    match canonicalized with
    | StartsWith "MENO" t -> conv 0 (t.Trim()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized
