module internal Ploeh.Numsense.Danish

open Ploeh.Numsense.InternalDsl

let rec internal toDanishImp magnitude x =
    let formatTens suffix factor x =
        let remainder = x % factor
        match remainder with
        | 0 -> suffix // 'tyve' instead of 'nul-og-tyve', and so on.
        | 1 -> sprintf "en-og-%s" suffix // 'en' instead of 'et'.
        | _ -> sprintf "%s-og-%s" (toDanishImp 1 remainder) suffix

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
        let hundreds = toDanishImp 100 (x / 100)
        match remainder with
        | 0 -> sprintf "%s-hundrede" hundreds
        | x when x < 20 && magnitude <= 100 ->
            sprintf "%s-hundrede-og-%s" hundreds (toDanishImp 10 x)
        | _ -> sprintf "%s-hundrede-%s" hundreds (toDanishImp 10 remainder)

    // When formatting the thousands, this function interleaves an 'og'
    // ('and') as a binding word when the remainder is small. As an
    // example, it creates 'to-tusind-og-halvtreds', but
    // 'to-tusind-tre-hundrede-tres'. To my native Danish ears, this sounds
    // natural, but is arbitrary and subjective; it isn't based on any
    // grammatical rule I know of.
    let formatThousands x =
        let remainder = x % 1000
        let thousands = toDanishImp 1000 (x / 1000)
        match remainder with
        | 0 -> sprintf "%s-tusind" thousands
        | x when x < 100 -> sprintf "%s-tusind-og-%s" thousands (toDanishImp 100 x)
        | _ -> sprintf "%s-tusind-%s" thousands (toDanishImp 100 remainder)

    // When formatting the millions, this function interleaves an 'og'
    // ('and') as a binding word when the remainder is small. As an
    // example, it creates 'to-millioner-og-seksten', but
    // 'to-millioner-tre-hundrede-firs'. To my native Danish ears, this
    // sounds natural, but is arbitrary and subjective; it isn't based on
    // any grammatical rule I know of.
    let formatMillions x =
        let remainder = x % 1000000
        let millions = x / 1000000
        let millionsText =
            if millions = 1
            then "en-million" // Not 'et-million', as imp would give.
            else sprintf "%s-millioner" (toDanishImp 1000000 millions)
        match remainder with
        | 0 -> millionsText
        | x when x < 100 -> sprintf "%s-og-%s" millionsText (toDanishImp 1000 x)
        | _ -> sprintf "%s-%s" millionsText (toDanishImp 1000 remainder)

    // When formatting the billions, this function interleaves an 'og'
    // ('and') as a binding word when the remainder is small. As an
    // example, it creates 'to-milliarder-og-seksten', but
    // 'to-milliarder-tre-hundrede-firs'. To my native Danish ears, this
    // sounds natural, but is arbitrary and subjective; it isn't based on
    // any grammatical rule I know of.
    let formatBillions x =
        let remainder = x % 1000000000
        let billions = x / 1000000000
        let billionsText =
            if billions = 1
            then "en-milliard" // Not 'et-milliard', as imp would give.
            else sprintf "%s-milliarder" (toDanishImp 1000000000 billions)
        match remainder with
        | 0 -> billionsText
        | x when x < 100 -> sprintf "%s-og-%s" billionsText (toDanishImp 1000000 x)
        | _ -> sprintf "%s-%s" billionsText (toDanishImp 1000000 remainder)

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toDanishImp 1 -x)
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
    | Between      20         30 x -> formatTens "tyve" 10 x
    | Between      30         40 x -> formatTens "tredive" 10 x
    | Between      40         50 x -> formatTens "fyrre" 10 x
    | Between      50         60 x -> formatTens "halvtreds" 10 x
    | Between      60         70 x -> formatTens "tres" 10 x
    | Between      70         80 x -> formatTens "halvfjerds" 10 x
    | Between      80         90 x -> formatTens "firs" 10 x
    | Between      90        100 x -> formatTens "halvfems" 10 x
    | Between     100       1000 x -> formatHundreds x
    | Between    1000    1000000 x -> formatThousands x
    | Between 1000000 1000000000 x -> formatMillions x
    | _                            -> formatBillions x

let internal tryParseDanishImp (x : string) =
    let rec conv acc candidate =
        match candidate with
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

    let canonicalized =
        x.Trim().ToUpper(System.Globalization.CultureInfo "da-DK")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized