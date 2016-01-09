module internal Ploeh.Numsense.German

open Ploeh.Numsense.InternalDsl

let rec internal toGermanImp x =

    // Esentially simplifies expressions like 'twenty-zero' to 'twenty',
    // 'one-milion-zero' to 'one-million', and so on.
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toGermanImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toGermanImp (x / factor)) suffix
        simplify prefix factor x

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toGermanImp -x)
    |  0 -> "null"
    |  1 -> "eins"
    |  2 -> "zwei"
    |  3 -> "drei"
    |  4 -> "vier"
    |  5 -> "fünf"
    |  6 -> "sechs"
    |  7 -> "sieben"
    |  8 -> "acht"
    |  9 -> "neun"
    | 10 -> "zehn"
    | 11 -> "elf"
    | 12 -> "zwölf"
    | 13 -> "dreizehn"
    | 14 -> "vierzehn"
    | 15 -> "fünfzehn"
    | 16 -> "sechszehn"
    | 17 -> "siebenzehn"
    | 18 -> "achtzehn"
    | 19 -> "neinzehn"
    | Between 20 30 x -> simplify "zwanzig" 10 x
    | Between 30 40 x -> simplify "dreisig" 10 x
    | Between 40 50 x -> simplify "vierzig" 10 x
    | Between 50 60 x -> simplify "fünfzig" 10 x
    | Between 80 90 x -> simplify "achtzig" 10 x
    | Between 60 100 x -> format "zig" 10 x
    | Between 100 1000 x -> format "-hundert" 100 x
    | Between 1000 1000000 x -> format "-tausend" 1000 x
    | Between 1000000 1000000000 x -> format "-million" 1000000 x
    | _ -> format "-milliarde" 1000000000 x

let internal tryParseGermanImp (x : string) =
    let rec conv acc (candidate : string) =
        let trimmed = candidate.Trim()
        match trimmed with
        | ""                      -> Some acc
        | StartsWith "-"         t
        | StartsWith "UND"       t -> conv                acc  t
        | StartsWith "NULL"      t -> conv          (0  + acc) t
        | StartsWith "EINE"      t -> conv          (1  + acc) t
        | StartsWith "EINS"      t -> conv          (1  + acc) t
        | StartsWith "EIN"       t -> conv          (1  + acc) t
        | StartsWith "ZWEI"      t -> conv          (2  + acc) t
        | StartsWith "ZEHN"      t -> conv         (10  + acc) t
        | StartsWith "ELF"       t -> conv         (11  + acc) t
        | StartsWith "ZWÖLF"     t -> conv         (12  + acc) t
        | StartsWith "DREIZEHN"  t -> conv         (13  + acc) t
        | StartsWith "FÜNFZEHN"  t -> conv         (15  + acc) t
        | StartsWith "EHN"       t // matches 'een' in 'eighteen'
        | StartsWith "ZEHN"      t -> conv         (10  + acc) t
        | StartsWith "ZWANZIG"   t -> conv         (20  + acc) t
        | StartsWith "DREIßIG"   t -> conv         (30  + acc) t
        | StartsWith "VIERZIG"   t -> conv         (40  + acc) t
        | StartsWith "FÜNFZIG"   t -> conv         (50  + acc) t
        | StartsWith "SECHZIG"   t -> conv         (60  + acc) t
        | StartsWith "SIEBZIG"   t -> conv         (70  + acc) t
        | StartsWith "ACHTZIG"   t -> conv         (80  + acc) t
        | StartsWith "NEUNZIG"   t -> conv         (90  + acc) t
        | StartsWith "DREI"      t -> conv          (3  + acc) t
        | StartsWith "VIER"      t -> conv          (4  + acc) t
        | StartsWith "FÜNF"      t -> conv          (5  + acc) t
        | StartsWith "SECHS"     t -> conv          (6  + acc) t
        | StartsWith "SIEBEN"    t -> conv          (7  + acc) t
        | StartsWith "ACHT"      t -> conv          (8  + acc) t
        | StartsWith "NEUN"      t -> conv          (9  + acc) t
        | StartsWith "HUNDERT"   t ->
            conv (if acc = 0 then  100 else         100 %* acc) t
        | StartsWith "TAUSEND"    t ->
            conv (if acc = 0 then 1000 else        1000 %* acc) t
        | StartsWith "MILLIONEN"  t -> conv    (1000000 %* acc) t
        | StartsWith "MILLION"    t -> conv    (1000000 %* acc) t
        | StartsWith "MILLIARDEN" t -> conv (1000000000  * acc) t
        | StartsWith "MILLIARDE"  t -> conv (1000000000  * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "de")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized
