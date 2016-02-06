module internal Ploeh.Numsense.Catalan

open Ploeh.Numsense.InternalDsl

let rec internal toCatalanImp x =

    // Esentially simplifies expressions like 'twenty-zero' to 'twenty',
    // 'one-milion-zero' to 'one-million', and so on.
    let simplify prefix infix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s%s%s" prefix infix (toCatalanImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toCatalanImp (x / factor)) suffix
        simplify prefix " " factor x

    match x with
    |  x when x < 0 -> sprintf "menys %s" (toCatalanImp -x)
    |  0 -> "zero"
    |  1 -> "un"
    |  2 -> "dos"
    |  3 -> "tres"
    |  4 -> "quatre"
    |  5 -> "cinc"
    |  6 -> "sis"
    |  7 -> "set"
    |  8 -> "vuit"
    |  9 -> "nou"
    | 10 -> "deu"
    | 11 -> "onze"
    | 12 -> "dotze"
    | 13 -> "tretze"
    | 14 -> "catorze"
    | 15 -> "quinze"
    | 16 -> "setze"
    | 17 -> "disset"
    | 18 -> "divuit"
    | 19 -> "dinou"
    | Between 20 30 x -> simplify "vint" "-i-" 10 x
    | Between 30 40 x -> simplify "trenta" "-" 10 x
    | Between 40 50 x -> simplify "quaranta" "-" 10 x
    | Between 50 60 x -> simplify "cinquanta" "-" 10 x
    | Between 60 70 x -> simplify "seixanta" "-" 10 x
    | Between 70 80 x -> simplify "setanta" "-" 10 x
    | Between 80 90 x -> simplify "vuitanta" "-" 10 x
    | Between 90 100 x -> simplify "noranta" "-" 10 x
    | Between 100 200 x -> simplify "cent" " " 100 x
    | Between 200 1000 x -> format "-cents" 100 x
    | Between 1000 2000 x -> simplify "mil" " " 1000 x
    | Between 2000 1000000 x -> format " mil" 1000 x
    | Between 1000000 2000000 x -> simplify "un milió" " " 1000000 x
    | _ -> format " milions" 1000000 x

let internal tryParseCatalanImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc
        | StartsWith " "        t
        | StartsWith "-I-"      t
        | StartsWith "-"        t -> conv                acc  t
        | StartsWith "ZERO"     t -> conv          (0  + acc) t
        | StartsWith "UN"       t -> conv          (1  + acc) t
        | StartsWith "DOS"      t -> conv          (2  + acc) t
        | StartsWith "TRES"     t -> conv          (3  + acc) t
        | StartsWith "QUATRE"   t -> conv          (4  + acc) t
        | StartsWith "CINC"     t -> conv          (5  + acc) t
        | StartsWith "SIS"      t -> conv          (6  + acc) t
        | StartsWith "NOU"      t -> conv          (9  + acc) t
        | StartsWith "DEU"      t -> conv         (10  + acc) t
        | StartsWith "ONZE"     t -> conv         (11  + acc) t
        | StartsWith "DOTZE"    t -> conv         (12  + acc) t
        | StartsWith "TRETZE"   t -> conv         (13  + acc) t
        | StartsWith "CATORZE"  t -> conv         (14  + acc) t
        | StartsWith "QUINZE"   t -> conv         (15  + acc) t
        | StartsWith "SETZE"    t -> conv         (16  + acc) t
        | StartsWith "DISSET"   t -> conv         (17  + acc) t
        | StartsWith "DIVUIT"   t -> conv         (18  + acc) t
        | StartsWith "DINOU"    t -> conv         (19  + acc) t
        | StartsWith "VINT"     t -> conv         (20  + acc) t
        | StartsWith "TRENTA"   t -> conv         (30  + acc) t
        | StartsWith "QUARANTA" t -> conv         (40  + acc) t
        | StartsWith "CINQUANTA" t -> conv        (50  + acc) t
        | StartsWith "SEIXANTA" t -> conv         (60  + acc) t
        | StartsWith "SETANTA"  t -> conv         (70  + acc) t
        | StartsWith "SET"      t -> conv          (7  + acc) t
        | StartsWith "VUITANTA" t -> conv         (80  + acc) t
        | StartsWith "VUIT"     t -> conv          (8  + acc) t
        | StartsWith "NORANTA"  t -> conv         (90  + acc) t
        | StartsWith "CENTS"    t -> conv        (100 %* acc) t
        | StartsWith "CENT"     t -> conv        (100  + acc) t
        | StartsWith "MILIONS"  t
        | StartsWith "MILIÓ"    t ->
            conv (if acc = 0 then 1000000 else 1000000 %* acc) t
        | StartsWith "MIL"      t ->
            conv (if acc = 0 then 1000    else   1000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "ca")
    match canonicalized with
    | StartsWith "MENYS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized
