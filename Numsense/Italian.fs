module internal Ploeh.Numsense.Italian

open Ploeh.Numsense.InternalDsl

let rec internal toItalianImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        match remainder with
        | 0 -> sprintf "%s" prefix
          // According to Italian grammar rules, if the word "tre"
          // appears at the end of another number, it needs the accent 
          // on the final "-e". Ex.: ventitre -> ventitré
          // It doesn't need the accent if "tre" is at the end of
          // a number but it's a separate word. Ex.: "un milione tre"
        | 3 when factor < 1000000 ->
            sprintf "%stré" prefix
        | _ when factor >= 1000000 -> 
            sprintf "%s %s" prefix (toItalianImp remainder)
        | _ -> sprintf "%s%s" prefix (toItalianImp remainder)

    let format suffix factor x =
        let num = x / factor
        let units = num % 10
        let leftPart = (num / 10) * 10
        let prefix = 
            match factor with
              // If a number ends with "3" but it is a multiplier for 
              // thousands, it doesn't need the accent. 
              // Ex.: 23000 = "ventitremila" (not "ventitrémila")
            | 1000 when units = 3 -> 
                if leftPart = 0 
                then sprintf "%s%s" (toItalianImp units) suffix
                else sprintf "%s%s%s" 
                        (toItalianImp leftPart) 
                        (toItalianImp units) 
                        suffix
            | _ -> sprintf "%s%s" (toItalianImp num) suffix
        simplify prefix factor x

    match x with
    |  x when x < 0 -> sprintf "meno %s" (toItalianImp -x)
    |  0 -> "zero"
    |  1 -> "uno"
    |  2 -> "due"
       // "tre", used as single word, doesn't need the accent.
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
    | Between 20 30 x ->  match x with
                          21 | 28 -> simplify "vent" 10 x
                          | _     -> simplify "venti" 10 x
    | Between 30 40 x ->  match x with
                          31 | 38 -> simplify "trent" 10 x
                          | _     -> simplify "trenta" 10 x
    | Between 40 50 x ->  match x with
                          41 | 48 -> simplify "quarant" 10 x
                          | _     -> simplify "quaranta" 10 x
    | Between 50 60 x ->  match x with
                          51 | 58 -> simplify "cinquant" 10 x
                          | _     -> simplify "cinquanta" 10 x
    | Between 60 70 x ->  match x with
                          61 | 68 -> simplify "sessant" 10 x
                          | _     -> simplify "sessanta" 10 x
    | Between 70 80 x ->  match x with
                          71 | 78 -> simplify "settant" 10 x
                          | _     -> simplify "settanta" 10 x
    | Between 80 90 x ->  match x with
                          81 | 88 -> simplify "ottant" 10 x
                          | _     -> simplify "ottanta" 10 x
    | Between 90 100 x -> match x with
                          91 | 98 -> simplify "novant" 10 x
                          | _     -> simplify "novanta" 10 x
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
          // Matches "un-" in "uno", "un milione" and "un miliardo"
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
        | StartsWith "TRÉ"          t
        | StartsWith "TRE'"         t
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
