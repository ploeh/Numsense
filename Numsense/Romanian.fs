module internal Ploeh.Numsense.Romanian

open Ploeh.Numsense.InternalDsl

let rec internal toRomanianImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toRomanianImp (remainder))

    let handleGender (s : string) factor =
        if s.EndsWith("doi") && factor >= 100 then s.Substring(0, s.Length - 3) + "două"
        elif s.StartsWith("doi") && factor >= 100 then "două" + s.Substring(3)
        elif s.EndsWith("unu") && factor = 1000 then s.Substring(0, s.Length - 3) + "una"
        else s

    let format suffix factor x =
        let converted = toRomanianImp (x / factor)

        let prefix = sprintf "%s%s" (handleGender converted factor) suffix
        simplify prefix factor x

    // Determines if the word "of" (in Romanian "de") should be added
    let isOfNeeded factor x =
        let quantifier = x / factor % 100
        match quantifier with
        | 0 -> true
        | Between 20 100 quantifier -> true
        | _ -> false

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toRomanianImp -x)
    |  0 -> "zero"
    |  1 -> "unu"
    |  2 -> "doi"
    |  3 -> "trei"
    |  4 -> "patru"
    |  5 -> "cinci"
    |  6 -> "șase"
    |  7 -> "șapte"
    |  8 -> "opt"
    |  9 -> "nouă"
    | 10 -> "zece"
    | 11 -> "unsprezece"
    | 12 -> "doisprezece"
    | 13 -> "treisprezece"
    | 14 -> "paisprezece"
    | 15 -> "cincisprezece"
    | 16 -> "șaisprezece"
    | 17 -> "șaptesprezece"
    | 18 -> "optsprezece"
    | 19 -> "nouăsprezece"
    | Between 20 21 x -> "douăzeci"
    | Between 21 30 x -> simplify "douăzeci-și" 10 x
    | Between 30 31 x -> "treizeci"
    | Between 31 40 x -> simplify "treizeci-și" 10 x
    | Between 40 41 x -> "patruzeci"
    | Between 41 50 x -> simplify "patruzeci-și" 10 x
    | Between 50 51 x -> "cincizeci"
    | Between 51 60 x -> simplify "cincizeci-și" 10 x
    | Between 60 61 x -> "șaizeci"
    | Between 61 70 x -> simplify "șaizeci-și" 10 x
    | Between 70 71 x -> "șaptezeci"
    | Between 71 80 x -> simplify "șaptezeci-și" 10 x
    | Between 80 81 x -> "optzeci"
    | Between 81 90 x -> simplify "optzeci-și" 10 x
    | Between 90 91 x -> "nouăzeci"
    | Between 91 100 x -> simplify "nouăzeci-și" 10 x
    | Between 100 200 x -> simplify "o-sută" 100 x
    | Between 200 1000 x -> format "-sute" 100 x
    | Between 1000 2000 x -> simplify "o-mie" 1000 x
    | Between 20000 1000000 x when isOfNeeded 1000 x -> format "-de-mii" 1000 x
    | Between 2000 1000000 x -> format "-mii" 1000 x
    | Between 1000000 2000000 x -> simplify "un-milion" 1000000 x
    | Between 20000000 1000000000 x when isOfNeeded 1000000 x -> format "-de-milioane" 1000000 x
    | Between 2000000 1000000000 x -> format "-milioane" 1000000 x
    | Between 1000000000 2000000000 x -> simplify "un-miliard" 1000000 x
    | _ -> format "-miliarde" 1000000000 x

let internal tryParseRomanianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc
        | StartsWith "-"           t
        | StartsWith "ȘI"          t
        | StartsWith "DE"          t -> conv                acc  t
        | StartsWith "ZERO"        t -> conv          (0  + acc) t
        | StartsWith "OPT"         t -> conv          (8  + acc) t
        | StartsWith "UNU"         t
        | StartsWith "UNA"         t
        | StartsWith "UN"          t
        | StartsWith "O"           t -> conv          (1  + acc) t
        | StartsWith "DOI"         t
        | StartsWith "DOUĂ"        t -> conv          (2  + acc) t
        | StartsWith "TREI"        t -> conv          (3  + acc) t
        | StartsWith "PATRU"       t -> conv          (4  + acc) t
        | StartsWith "CINCI"       t -> conv          (5  + acc) t
        | StartsWith "ȘASE"        t -> conv          (6  + acc) t
        | StartsWith "ȘAPTE"       t -> conv          (7  + acc) t
        | StartsWith "NOUĂ"        t -> conv          (9  + acc) t
        | StartsWith "ZECE"        t -> conv         (10  + acc) t
        | StartsWith "UNSPREZECE"  t -> conv         (11  + acc) t
        | StartsWith "PAISPREZECE" t -> conv         (14  + acc) t
        | StartsWith "ȘAISPREZECE" t -> conv         (16  + acc) t
        | StartsWith "SPREZECE"    t -> conv         (10  + acc) t
        | StartsWith "ȘAIZECI"     t -> conv         (60  + acc) t
        | StartsWith "ZECI"        t -> conv         (10 %* acc) t
        | StartsWith "SUTĂ"        t
        | StartsWith "SUTE"        t ->
            conv (if acc = 0 then  100 else          100 %* acc) t
        | StartsWith "MIE"         t
        | StartsWith "MII"         t ->
            conv (if acc = 0 then 1000 else         1000 %* acc) t
        | StartsWith "MILION"      t
        | StartsWith "MILIOANE"    t -> conv    (1000000 %* acc) t
        | StartsWith "MILIARDE"    t
        | StartsWith "MILIARD"     t -> conv  (1000000000 * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "ro-RO")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized
