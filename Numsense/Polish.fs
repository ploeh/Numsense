module internal Ploeh.Numsense.Polish

open Ploeh.Numsense.InternalDsl

let rec internal toPolishImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toPolishImp remainder)

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toPolishImp (x / factor)) suffix
        simplify prefix factor x

    let formatTens x =
        let tens = x / 10
        match x with
        | 10 -> "dziesięć"
        | 11 -> "jedenaście"
        | 12 -> "dwanaście"
        | 13 -> "trzynaście"
        | 14 -> "czternaście"
        | 15 -> "piętnaście"
        | 16 -> "szesnaście"
        | 17 -> "siedemnaście"
        | 18 -> "osiemnaście"
        | 19 -> "dziewiętnaście"
        | _ -> match tens with
               | 2 -> simplify "dwadzieścia" 10 x
               | 4 -> simplify "czterdzieści" 10 x
               | tens when tens < 5 -> simplify (format "dzieści" 1 tens) 10 x
               | _ -> simplify (format "dziesiąt" 1 tens) 10 x

    let formatHundreds x =
        let hundreds = x / 100
        match hundreds with
        | 1 -> simplify "sto" 100 x
        | 2 -> simplify "dwieście" 100 x
        | hundreds when hundreds < 5 -> format "sta" 100 x
        | _ -> format "set" 100 x

    let formatNumerals single plural1 plural2 factor x =
        let thousands = x / factor
        let remainder = thousands % 10
        match thousands with
        | 1 -> simplify single factor x
        | thousands when thousands > 9 && thousands < 20 -> format plural1 factor x
        | _ -> match remainder with
               | remainder when remainder > 1 && remainder < 5 -> format plural2 factor x
               | _ -> format plural1 factor x

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toPolishImp -x)
    |  0 -> "zero"
    |  1 -> "jeden"
    |  2 -> "dwa"
    |  3 -> "trzy"
    |  4 -> "cztery"
    |  5 -> "pięć"
    |  6 -> "sześć"
    |  7 -> "siedem"
    |  8 -> "osiem"
    |  9 -> "dziewięć"
    | Between 10 100 x -> formatTens x
    | Between 100 1000 x -> formatHundreds x
    | Between 1000 1000000 x -> formatNumerals "tysiąc" "-tysięcy" "-tysiące" 1000 x
    | Between 1000000 1000000000 x -> formatNumerals "milion" "-milionów" "-miliony" 1000000 x
    | _ -> formatNumerals "miliard" "-miliardów" "-miliardy" 1000000000 x

let internal tryParsePolishImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc
        | StartsWith "-"              t
        | StartsWith "ZERO"           t -> conv           (0  + acc) t
        | StartsWith "JEDENAŚCIE"     t -> conv           (11 + acc) t
        | StartsWith "JEDEN"          t -> conv           (1  + acc) t
        | StartsWith "DWA"            t -> conv           (2  + acc) t
        | StartsWith "TRZY"           t -> conv           (3  + acc) t
        | StartsWith "CZTERY"         t -> conv           (4  + acc) t
        | StartsWith "PIĘĆ"           t -> conv           (5  + acc) t
        | StartsWith "SZEŚĆ"          t -> conv           (6  + acc) t
        | StartsWith "SIEDEM"         t -> conv           (7  + acc) t
        | StartsWith "OSIEM"          t -> conv           (8  + acc) t
        | StartsWith "DZIEWIĘĆ"       t -> conv           (9  + acc) t
        | StartsWith "DZIESIĘĆ"       t -> conv          (10  + acc) t
        | StartsWith "CZTERNAŚCIE"    t -> conv          (14  + acc) t
        | StartsWith "PIĘTNAŚCIE"     t -> conv          (15  + acc) t
        | StartsWith "SZESNAŚCIE"     t -> conv          (16  + acc) t
        | StartsWith "DZIEWIĘTNAŚCIE" t -> conv          (19  + acc) t
        | StartsWith "NAŚCIE"         t -> conv          (10  + acc) t
        | StartsWith "CZTERDZIEŚCI"   t -> conv          (40  + acc) t
        | StartsWith "DZIEŚCIA"       t -> conv          (10 %* acc) t
        | StartsWith "DZIEŚCI"        t -> conv          (10 %* acc) t
        | StartsWith "DZIESIĄT"       t -> conv          (10 %* acc) t
        | StartsWith "STO"            t -> conv          (100 + acc) t
        | StartsWith "DWIEŚCIE"       t -> conv          (200 + acc) t
        | StartsWith "STA"            t -> conv         (100 %* acc) t
        | StartsWith "SET"            t -> conv         (100 %* acc) t
        | StartsWith "TYSIĄCE"        t -> conv        (1000 %* acc) t
        | StartsWith "TYSIĘCY"        t -> conv        (1000 %* acc) t
        | StartsWith "MILIONY"        t -> conv     (1000000 %* acc) t
        | StartsWith "MILIONÓW"       t -> conv     (1000000 %* acc) t
        | StartsWith "MILIARDY"       t -> conv  (1000000000 %* acc) t
        | StartsWith "MILIARDÓW"      t -> conv  (1000000000 %* acc) t
        | StartsWith "TYSIĄC"         t ->
            conv (if acc = 0 then       1000 else       1000 %* acc) t
        | StartsWith "MILION"         t ->
            conv (if acc = 0 then    1000000 else    1000000 %* acc) t
        | StartsWith "MILIARD"        t ->
            conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "pl")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized