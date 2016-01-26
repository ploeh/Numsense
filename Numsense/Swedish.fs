module internal Ploeh.Numsense.Swedish

open Ploeh.Numsense.InternalDsl

let rec internal toSwedishImp x =
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toSwedishImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toSwedishImp (x / factor)) suffix
        simplify prefix factor x

    let formatMillions x =
        let multiplier = x / 1000000
        let prefix =
            if multiplier = 1 then
                "en-miljon"
            else
                let multiplierUnit =
                    match multiplier with
                    | Between 21 1000 x -> x % 10
                    | _ -> multiplier
                if multiplierUnit = 1 then
                    sprintf "%s-en-miljoner" (toSwedishImp (multiplier - 1))
                else
                    sprintf "%s-miljoner" (toSwedishImp multiplier)
        simplify prefix 1000000 x

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toSwedishImp -x)
    |  0 -> "noll"
    |  1 -> "ett"
    |  2 -> "två"
    |  3 -> "tre"
    |  4 -> "fyra"
    |  5 -> "fem"
    |  6 -> "sex"
    |  7 -> "sju"
    |  8 -> "åtta"
    |  9 -> "nio"
    | 10 -> "tio"
    | 11 -> "elva"
    | 12 -> "tolv"
    | 13 | 17 -> format "tton" 1 (x % 10)
    | 14 -> "fjorton"
    | 15 | 16 -> format "ton" 1 (x % 10)
    | 18 -> "arton"
    | 19 -> "nitton"
    | Between 20 30 x -> simplify "tjugo" 10 x
    | Between 30 40 x -> simplify "trettio" 10 x
    | Between 40 50 x -> simplify "fyrtio" 10 x
    | Between 50 70 x -> format "tio" 10 x
    | Between 70 80 x -> simplify "sjuttio" 10 x
    | Between 80 90 x -> simplify "åttio" 10 x
    | Between 90 100 x -> simplify "nittio" 10 x
    | Between 100 1000 x -> format "-hundra" 100 x
    | Between 1000 1000000 x -> format "-tusen" 1000 x
    | Between 1000000 1000000000 x -> formatMillions x
    | Between 1000000000 2000000000 x -> simplify "en-miljard" 1000000000 x
    | _ -> format "-miljarder" 1000000000 x

let internal tryParseSwedishImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                       -> Some acc
        | StartsWith "-"         t
        | StartsWith "OCH"       t -> conv                acc  t
        | StartsWith "NOLL"      t -> conv          (0  + acc) t
        | StartsWith "EN"        t
        | StartsWith "ETT"       t -> conv          (1  + acc) t
        | StartsWith "TVÅ"       t -> conv          (2  + acc) t
        | StartsWith "TRE"       t -> conv          (3  + acc) t
        | StartsWith "FYRA"      t -> conv          (4  + acc) t
        | StartsWith "FEMTIO"    t -> conv         (50  + acc) t
        | StartsWith "FEM"       t -> conv          (5  + acc) t
        | StartsWith "SEXTIO"    t -> conv         (60  + acc) t
        | StartsWith "SEX"       t -> conv          (6  + acc) t
        | StartsWith "SJU"       t -> conv          (7  + acc) t
        | StartsWith "ÅTTA"      t -> conv          (8  + acc) t
        | StartsWith "NIO"       t -> conv          (9  + acc) t
        | StartsWith "TIO"       t -> conv         (10  + acc) t
        | StartsWith "ELVA"      t -> conv         (11  + acc) t
        | StartsWith "TOLV"      t -> conv         (12  + acc) t
        | StartsWith "TTON"      t
        | StartsWith "TON"       t -> conv         (10  + acc) t
        | StartsWith "FJORTON"   t -> conv         (14  + acc) t
        | StartsWith "ARTON"     t -> conv         (18  + acc) t
        | StartsWith "NITTON"    t -> conv         (19  + acc) t
        | StartsWith "TJUGO"     t -> conv         (20  + acc) t
        | StartsWith "TTIO"      t -> conv         (10 %* acc) t
        | StartsWith "FYRTIO"    t -> conv         (40  + acc) t
        | StartsWith "ÅTTIO"     t -> conv         (80  + acc) t
        | StartsWith "NITTIO"    t -> conv         (90  + acc) t
        | StartsWith "HUNDRA"    t ->
            conv (if acc = 0 then  100 else        100 %* acc) t
        | StartsWith "USEN"      t
        | StartsWith "TUSEN"     t ->
            conv (if acc = 0 then 1000 else       1000 %* acc) t
        | StartsWith "MILJONER"  t
        | StartsWith "MILJON"    t -> conv    (1000000 %* acc) t
        | StartsWith "MILJARDER" t
        | StartsWith "MILJARD"   t -> conv (1000000000  * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "sv-SE")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized