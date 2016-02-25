module internal Ploeh.Numsense.Spanish

open Ploeh.Numsense.InternalDsl

let rec internal toSpanishImp x =

    // Esentially simplifies expressions like 'twenty-zero' to 'twenty',
    // 'one-milion-zero' to 'one-million', and so on.
    let simplify prefix infix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s%s%s" prefix infix (toSpanishImp (remainder))

    let apocopatePrefix (prefix: string) =
      if (prefix.Contains("veintiuno")) then
        prefix.Replace(" veintiuno ", " veintiún ")
      else
        prefix.Replace(" uno ", " un ")

    let format suffix infix apocopate factor x =
        let prefix = sprintf "%s%s%s" (toSpanishImp (x / factor)) infix suffix
        if apocopate then
          simplify (apocopatePrefix prefix) " " factor x
        else
          simplify prefix " " factor x

    match x with
    |  x when x < 0 -> sprintf "menos %s" (toSpanishImp -x)
    |  0 -> "cero"
    |  1 -> "uno"
    |  2 -> "dos"
    |  3 -> "tres"
    |  4 -> "cuatro"
    |  5 -> "cinco"
    |  6 -> "seis"
    |  7 -> "siete"
    |  8 -> "ocho"
    |  9 -> "nueve"
    | 10 -> "diez"
    | 11 -> "once"
    | 12 -> "doce"
    | 13 -> "trece"
    | 14 -> "catorce"
    | 15 -> "quince"
    | 16 -> "dieciséis"
    | Between 17 20 x -> simplify "dieci" "" 10 x
    | 20 -> "veinte"
    | 22 -> "veintidós"
    | 23 -> "veintitrés"
    | 26 -> "veintiséis"
    | Between 20 30 x -> simplify "veinti" "" 10 x
    | Between 30 40 x -> simplify "treinta" " y " 10 x
    | Between 40 50 x -> simplify "cuarenta" " y " 10 x
    | Between 50 60 x -> simplify "cincuenta" " y " 10 x
    | Between 60 70 x -> simplify "sesenta" " y " 10 x
    | Between 70 80 x -> simplify "setenta" " y " 10 x
    | Between 80 90 x -> simplify "ochenta" " y " 10 x
    | Between 90 100 x -> simplify "noventa" " y " 10 x
    | 100 -> "cien"
    | Between 101 200 x -> simplify "ciento" " " 100 x
    | Between 200 500 x -> format "cientos" "" false 100 x
    | Between 500 600 x -> simplify "quinientos" " " 100 x
    | Between 600 700 x -> format "cientos" "" false 100 x
    | Between 700 800 x -> simplify "setecientos" " " 100 x
    | Between 800 900 x -> format "cientos" "" false 100 x
    | Between 900 1000 x -> simplify "novecientos" " " 100 x
    | Between 1000 2000 x -> simplify "mil" " " 1000 x
    | Between 2000 101000 x -> format "mil" " " true 1000 x
    | Between 2000 1000000 x -> format "mil" " " true 1000 x
    | Between 1000000 2000000 x -> simplify "un millón" " " 1000000 x
    | _ -> format "millones" " " true 1000000 x

let internal tryParseSpanishImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                      -> Some acc
        | StartsWith " "        t
        | StartsWith "Y"        t -> conv                acc  t
        | StartsWith "CERO"     t -> conv          (0  + acc) t
        | StartsWith "UNO"      t
        | StartsWith "UN"       t
        | StartsWith "ÚN"       t -> conv          (1  + acc) t
        | StartsWith "DOS"      t
        | StartsWith "DÓS"      t -> conv          (2  + acc) t
        | StartsWith "TRÉS"     t
        | StartsWith "TRES"     t -> conv          (3  + acc) t
        | StartsWith "CUATRO"   t -> conv          (4  + acc) t
        | StartsWith "QUINI"    t
        | StartsWith "CINCO"    t -> conv          (5  + acc) t
        | StartsWith "SÉIS"     t
        | StartsWith "SEIS"     t -> conv          (6  + acc) t
        | StartsWith "SETECI"   t
        | StartsWith "SIETE"    t -> conv          (7  + acc) t
        | StartsWith "OCHO"     t -> conv          (8  + acc) t
        | StartsWith "NOVECI"   t
        | StartsWith "NUEVE"    t -> conv          (9  + acc) t
        | StartsWith "DIEZ"     t -> conv         (10  + acc) t
        | StartsWith "ONCE"     t -> conv         (11  + acc) t
        | StartsWith "DOCE"     t -> conv         (12  + acc) t
        | StartsWith "TRECE"    t -> conv         (13  + acc) t
        | StartsWith "CATORCE"  t -> conv         (14  + acc) t
        | StartsWith "QUINCE"   t -> conv         (15  + acc) t
        | StartsWith "DIECI"    t -> conv         (10  + acc) t
        | StartsWith "VEINTE"   t -> conv         (20  + acc) t
        | StartsWith "VEINTI"   t -> conv         (20  + acc) t
        | StartsWith "TREINTA"  t -> conv         (30  + acc) t
        | StartsWith "CUARENTA" t -> conv         (40  + acc) t
        | StartsWith "CINCUENTA" t -> conv        (50  + acc) t
        | StartsWith "SESENTA"  t -> conv         (60  + acc) t
        | StartsWith "SETENTA"  t -> conv         (70  + acc) t
        | StartsWith "OCHENTA"  t -> conv         (80  + acc) t
        | StartsWith "NOVENTA"  t -> conv         (90  + acc) t
        | StartsWith "CIENTOS"  t
        | StartsWith "ENTOS"    t -> // From quini-entos
            conv (if acc = 0 then  100 else       100 %* acc) t
        | StartsWith "CIENTO"   t
        | StartsWith "CIEN"     t -> conv         (100 + acc) t
        | StartsWith "MILLÓN"   t
        | StartsWith "MILLONES" t -> conv    (1000000 %* acc) t
        | StartsWith "MILLARDOS"  t
        | StartsWith "MILLARDO" t -> conv (1000000000  * acc) t
        | StartsWith "MIL"      t ->
            conv (if acc = 0 then 1000 else      1000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "es")
    match canonicalized with
    | StartsWith "MENOS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized