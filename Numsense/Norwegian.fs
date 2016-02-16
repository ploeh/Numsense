module internal Ploeh.Numsense.Norwegian

open Ploeh.Numsense.InternalDsl

let rec internal toNorwegianImp magnitude x =

    let formatTens prefix factor x =
        let remainder = x % factor
        match remainder with
        | 0 -> prefix // 'tjue' instead of 'tjue-null', and so on.
        | 1 -> sprintf "%s-en" prefix // 'en' instead of 'ett'.
        | _ -> sprintf "%s-%s" prefix (toNorwegianImp 1 remainder)

    let formatHundreds x =
        let remainder = x % 100
        let hundreds = toNorwegianImp 100 (x / 100)
        match remainder with
        | 0 -> sprintf "%s-hundre" hundreds
        | 1 -> sprintf "%s-hundre-og-en" hundreds // 'en' instead of 'ett'.
        | _ -> sprintf "%s-hundre-og-%s" hundreds (toNorwegianImp 10 remainder)

    let formatThousands x =
        let remainder = x % 1000
        let thousands = toNorwegianImp 1000 (x / 1000)
        match remainder with
        | 0 -> sprintf "%s-tusen" thousands
        | 1 -> sprintf "%s-tusen-og-en" thousands // 'en' instead of 'ett'.
        | x when x < 100 -> sprintf "%s-tusen-og-%s" thousands (toNorwegianImp 100 x)
        | _ -> sprintf "%s-tusen-%s" thousands (toNorwegianImp 100 remainder)

    let formatMillions x =
        let remainder = x % 1000000
        let millions = x / 1000000
        let millionsText =
            if millions = 1
            then "en-million" // Not 'ett-million', as imp would give.
            else sprintf "%s-millioner" (toNorwegianImp 1000000 millions)
        match remainder with
        | 0 -> millionsText
        | 1 -> sprintf "%s-og-en" millionsText  // 'en' instead of 'ett'.
        | x when x < 100 -> sprintf "%s-og-%s" millionsText (toNorwegianImp 1000 x)
        | _ -> sprintf "%s-%s" millionsText (toNorwegianImp 1000 remainder)

    let formatBillions x =
        let remainder = x % 1000000000
        let billions = x / 1000000000
        let billionsText =
            if billions = 1
            then "en-milliard" // Not 'ett-milliard', as imp would give.
            else sprintf "%s-milliarder" (toNorwegianImp 1000000000 billions)
        match remainder with
        | 0 -> billionsText
        | 1 -> sprintf "%s-og-en" billionsText  // 'en' instead of 'ett'.
        | x when x < 100 -> sprintf "%s-og-%s" billionsText (toNorwegianImp 1000000 x)
        | _ -> sprintf "%s-%s" billionsText (toNorwegianImp 1000000 remainder)


   
    match x with
    |  x when x < 0 -> sprintf "minus %s" (toNorwegianImp 1 -x)
    |  0 -> "null"
    |  1 -> "ett"
    |  2 -> "to"
    |  3 -> "tre"
    |  4 -> "fire"
    |  5 -> "fem"
    |  6 -> "seks"
    |  7 -> "sju"
    |  8 -> "åtte"
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
    | Between 20 30 x -> formatTens "tjue" 10 x
    | Between 30 40 x -> formatTens "tretti" 10 x
    | Between 40 50 x -> formatTens "førti" 10 x
    | Between 50 60 x -> formatTens "femti" 10 x
    | Between 60 70 x -> formatTens "seksti" 10 x
    | Between 70 80 x -> formatTens "sytti" 10 x
    | Between 80 90 x -> formatTens "åtti" 10 x
    | Between 90 100 x -> formatTens "nitti" 10 x
    | Between     100       1000 x -> formatHundreds x
    | Between    1000    1000000 x -> formatThousands x
    | Between 1000000 1000000000 x -> formatMillions x
    | _                            -> formatBillions x

let internal tryParseNorwegianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc
        | StartsWith "-"        t
        | StartsWith "OG"      t -> conv                acc  t
        | StartsWith "NULL"     t -> conv          (0  + acc) t
        | StartsWith "EN"      t -> conv          (1  + acc) t
        | StartsWith "ETT"      t -> conv          (1  + acc) t
        | StartsWith "TOLV"   t -> conv         (12  + acc) t
        | StartsWith "TO"      t -> conv          (2  + acc) t
        | StartsWith "TRETTEN" t -> conv         (13  + acc) t
        | StartsWith "TRETTI"   t -> conv         (30  + acc) t
        | StartsWith "TRE"    t -> conv          (3  + acc) t
        | StartsWith "FIRE"     t -> conv          (4  + acc) t
        | StartsWith "FEMTEN"  t -> conv         (15  + acc) t
        | StartsWith "FEMTI"    t -> conv         (50  + acc) t
        | StartsWith "FEM"     t -> conv          (5  + acc) t
        | StartsWith "SEKSTEN"  t -> conv         (16  + acc) t
        | StartsWith "SEKSTI"    t -> conv         (60  + acc) t
        | StartsWith "SEKS"      t -> conv          (6  + acc) t
        | StartsWith "SJU"    t -> conv          (7  + acc) t
        | StartsWith "ÅTTE"    t -> conv          (8  + acc) t
        | StartsWith "NITTEN"  t -> conv         (19  + acc) t
        | StartsWith "NITTI"    t -> conv         (90  + acc) t
        | StartsWith "NI"     t -> conv          (9  + acc) t
        | StartsWith "TI"      t -> conv         (10  + acc) t
        | StartsWith "ELLEVE"   t -> conv         (11  + acc) t
        | StartsWith "FJORTEN" t -> conv         (14  + acc) t
        | StartsWith "SYTTEN"  t -> conv         (17  + acc) t
        | StartsWith "ATTEN"  t -> conv         (18  + acc) t
        | StartsWith "TJUE"   t -> conv         (20  + acc) t
        | StartsWith "FØRTI"    t -> conv         (40  + acc) t
        | StartsWith "SYTTI"    t -> conv         (70  + acc) t
        | StartsWith "ÅTTI"    t -> conv         (80  + acc) t
        | StartsWith "HUNDRE"  t ->
            conv (if acc = 0 then  100 else       100 %* acc) t
        | StartsWith "TUSEN" t ->
            conv (if acc = 0 then 1000 else      1000 %* acc) t
        | StartsWith "MILLION"  t -> conv    (1000000 %* acc) t
        | StartsWith "BILLION"  t -> conv (1000000000  * acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "nb-NO")
    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized