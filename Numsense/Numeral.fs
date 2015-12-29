module Ploeh.Numsense.Numeral

let private (|StartsWith|_|) prefix (candidate : string) =
    if candidate.StartsWith prefix
    then Some (candidate.Substring prefix.Length)
    else None

let private (|Between|_|) lower upper candidate =
    if lower <= candidate && candidate < upper
    then Some candidate
    else None

let private (%*) factor x =
    let multiplicand = x % factor
    x + (factor * multiplicand) - multiplicand

let toDanish x =

    let rec imp magnitude x =
        let formatTens suffix factor x =
            let remainder = x % factor
            match remainder with
            | 0 -> suffix // 'tyve' instead of 'nul-og-tyve', and so on.
            | 1 -> sprintf "en-og-%s" suffix // 'en' instead of 'et'.
            | _ -> sprintf "%s-og-%s" (imp 1 remainder) suffix

        // When formatting the hundreds, this function interleaves an 'og'
        // ('and') as a binding word when the remainder is small. As an
        // example, it creates 'to-hundrede-og-elleve', but
        // 'tre-hundrede-halvtreds'. To my native Danish ears, this sounds
        // natural, but is arbitrary and subjective; it isn't based on any
        // grammatical rule I know of.
        //
        // Correspondingly, when formatting the hundreds from a larger
        // magnitude (to quantify thousands or millions), it never feels right
        // to include the binding 'og'; as an example,
        // 'to-hundrede-og-to-tusind-og-to' doesn't sound as correct as
        // 'to-hundrede-to-tusind-og-to'.
        let formatHundreds x =
            let remainder = x % 100
            let hundreds = imp 100 (x / 100)
            match remainder with
            | 0 -> sprintf "%s-hundrede" hundreds
            | x when x < 20 && magnitude <= 100 ->
                sprintf "%s-hundrede-og-%s" hundreds (imp 10 x)
            | _ -> sprintf "%s-hundrede-%s" hundreds (imp 10 remainder)

        // When formatting the thousands, this function interleaves an 'og'
        // ('and') as a binding word when the remainder is small. As an
        // example, ti creates 'to-tusind-og-halvtreds', but
        // 'to-tusind-tre-hundrede-tres'. To my native Danish ears, this sounds
        // natural, but is arbitrary and subjective; it isn't based on any
        // grammatical rule I know of.
        let formatThousands x =
            let remainder = x % 1000
            let thousands = imp 1000 (x / 1000)
            match remainder with
            | 0 -> sprintf "%s-tusind" thousands
            | x when x < 100 -> sprintf "%s-tusind-og-%s" thousands (imp 100 x)
            | _ -> sprintf "%s-tusind-%s" thousands (imp 100 remainder)

        match x with
        |  0 -> "nul"
        |  1 -> "et"
        |  2 -> "to"
        |  3 -> "tre"
        |  4 -> "fire"
        |  5 -> "fem"
        |  6 -> "seks"
        |  7 -> "syv"
        |  8 -> "otte"
        |  9 -> "ni"
        | 10 -> "ti"
        | 11 -> "elleve"
        | 12 -> "tolv"
        | 13 -> "tretten"
        | 14 -> "fjorten"
        | 15 -> "femten"
        | 16 -> "seksten"
        | 17 -> "sytten"
        | 18 -> "atten"
        | 19 -> "nitten"    
        | Between   20      30 x -> formatTens "tyve" 10 x
        | Between   30      40 x -> formatTens "tredive" 10 x
        | Between   40      50 x -> formatTens "fyrre" 10 x
        | Between   50      60 x -> formatTens "halvtreds" 10 x
        | Between   60      70 x -> formatTens "tres" 10 x
        | Between   70      80 x -> formatTens "halvfjerds" 10 x
        | Between   80      90 x -> formatTens "firs" 10 x
        | Between   90     100 x -> formatTens "halvfems" 10 x
        | Between  100    1000 x -> formatHundreds x
        | Between 1000 1000000 x -> formatThousands x
        |  _ -> string x

    imp 1 x

let tryOfDanish x =
    let rec conv acc xs =
        match xs with
        | ""                        -> Some acc
        | StartsWith " "          t
        | StartsWith "-"          t
        | StartsWith "OG"         t -> conv                acc  t
        | StartsWith "NUL"        t -> conv          (0  + acc) t
        | StartsWith "ET"         t
        | StartsWith "EN"         t -> conv          (1  + acc) t
        | StartsWith "TO"         t -> conv          (2  + acc) t
        | StartsWith "TREDIVE"    t -> conv         (30  + acc) t
        | StartsWith "TRES"       t -> conv         (60  + acc) t
        | StartsWith "TRE"        t -> conv          (3  + acc) t
        | StartsWith "FIRE"       t -> conv          (4  + acc) t
        | StartsWith "FEM"        t -> conv          (5  + acc) t
        | StartsWith "SEKS"       t -> conv          (6  + acc) t
        | StartsWith "SYV"        t -> conv          (7  + acc) t
        | StartsWith "OTTE"       t -> conv          (8  + acc) t
        | StartsWith "NI"         t -> conv          (9  + acc) t
        | StartsWith "TI"         t -> conv         (10  + acc) t
        | StartsWith "ELLEVE"     t -> conv         (11  + acc) t
        | StartsWith "FJORTEN"    t -> conv         (14  + acc) t
        | StartsWith "SYTTEN"     t -> conv         (17  + acc) t
        | StartsWith "ATTEN"      t -> conv         (18  + acc) t
        | StartsWith "LV"         t // Matches 'lv' in 'tolv'
        | StartsWith "TTEN"       t // Matches 'tten' in 'tretten' and 'nitten'
        | StartsWith "TEN"        t -> conv         (10  + acc) t
        | StartsWith "TYVE"       t -> conv         (20  + acc) t        
        | StartsWith "FYRRE"      t -> conv         (40  + acc) t
        | StartsWith "HALVTREDS"  t -> conv         (50  + acc) t
        | StartsWith "HALVFJERDS" t -> conv         (70  + acc) t
        | StartsWith "FIRS"       t -> conv         (80  + acc) t
        | StartsWith "HALVFEMS"   t -> conv         (90  + acc) t
        | StartsWith "HUNDREDE"   t ->
            conv (if acc = 0 then  100 else         100 %* acc) t
        | StartsWith "TUSIND"     t ->
            conv (if acc = 0 then 1000 else        1000 %* acc) t
        | StartsWith "MILLIONER"  t
        | StartsWith "MILLION"    t -> conv    (1000000 %* acc) t
        | StartsWith "MILLIARDER" t
        | StartsWith "MILLIARD"   t -> conv (1000000000  * acc) t
        | _ -> None

    match System.Int32.TryParse x with
    | true, i -> Some i
    | _ -> conv 0 (x.Trim().ToUpper(System.Globalization.CultureInfo "da-DK"))

let rec toEnglish x =

    // Esentially simplifies expressions like 'twenty-zero' to 'twenty',
    // 'one-milion-zero' to 'one-million', and so on.
    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toEnglish (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toEnglish (x / factor)) suffix
        simplify prefix factor x

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toEnglish -x)
    |  0 -> "zero"
    |  1 -> "one"
    |  2 -> "two"
    |  3 -> "three"
    |  4 -> "four"
    |  5 -> "five"
    |  6 -> "six"
    |  7 -> "seven"
    |  8 -> "eight"
    |  9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | Between 20 30 x -> simplify "twenty" 10 x
    | Between 30 40 x -> simplify "thirty" 10 x
    | Between 40 50 x -> simplify "forty" 10 x
    | Between 50 60 x -> simplify "fifty" 10 x
    | Between 80 90 x -> simplify "eighty" 10 x
    | Between 60 100 x -> format "ty" 10 x
    | Between 100 1000 x -> format "-hundred" 100 x
    | Between 1000 1000000 x -> format "-thousand" 1000 x
    | Between 1000000 1000000000 x -> format "-million" 1000000 x
    | _ -> format "-billion" 1000000000 x

let tryOfEnglish (x : string) =
    let rec conv acc xs =        
        match xs with
        | ""                      -> Some acc
        | StartsWith "-"        t
        | StartsWith "AND"      t -> conv                acc  t
        | StartsWith "ZERO"     t -> conv          (0  + acc) t
        | StartsWith "ONE"      t -> conv          (1  + acc) t
        | StartsWith "TWO"      t -> conv          (2  + acc) t
        | StartsWith "THREE"    t -> conv          (3  + acc) t
        | StartsWith "FOUR"     t -> conv          (4  + acc) t
        | StartsWith "FIVE"     t -> conv          (5  + acc) t
        | StartsWith "SIX"      t -> conv          (6  + acc) t
        | StartsWith "SEVEN"    t -> conv          (7  + acc) t
        | StartsWith "EIGHT"    t -> conv          (8  + acc) t
        | StartsWith "NINE"     t -> conv          (9  + acc) t
        | StartsWith "TEN"      t -> conv         (10  + acc) t
        | StartsWith "ELEVEN"   t -> conv         (11  + acc) t
        | StartsWith "TWELVE"   t -> conv         (12  + acc) t
        | StartsWith "THIRTEEN" t -> conv         (13  + acc) t
        | StartsWith "FIFTEEN"  t -> conv         (15  + acc) t
        | StartsWith "EEN"      t // matches 'een' in 'eighteen'         
        | StartsWith "TEEN"     t -> conv         (10  + acc) t
        | StartsWith "TWENTY"   t -> conv         (20  + acc) t
        | StartsWith "THIRTY"   t -> conv         (30  + acc) t
        | StartsWith "FORTY"    t -> conv         (40  + acc) t
        | StartsWith "FIFTY"    t -> conv         (50  + acc) t
        | StartsWith "Y"        t // matches 'y' in 'eighty'
        | StartsWith "TY"       t -> conv         (10 %* acc) t
        | StartsWith "HUNDRED"  t ->
            conv (if acc = 0 then  100 else       100 %* acc) t
        | StartsWith "THOUSAND" t ->
            conv (if acc = 0 then 1000 else      1000 %* acc) t
        | StartsWith "MILLION"  t -> conv    (1000000 %* acc) t
        | StartsWith "BILLION"  t -> conv (1000000000  * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "en")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized