module internal Ploeh.Numsense.Dutch

open Ploeh.Numsense.InternalDsl

let rec internal toDutchImp x : string =

    let formatTens suffix factor x =
        let remainder = x % factor
        if remainder = 0
        then suffix
        else 
            let (remainderStr: string) = toDutchImp (remainder)
            let infix = if remainderStr.EndsWith("e") then "ën" else "en"
            sprintf "%s%s%s" remainderStr infix suffix 

    let format infix factor x =
        let div = x / factor        
        let rem = x % factor
        let suffixPrefix = if rem > 0 && x >= 1000 then " " else ""
        let prefixSuffix = if x >= 1000000 then " " else ""
        let suffix = if rem = 0 then "" else suffixPrefix + toDutchImp rem
        let prefix = if div = 1 && x < 1000000 then "" else toDutchImp div + prefixSuffix
        sprintf "%s%s%s" prefix infix suffix

    match x with
    |  x when x < 0 -> sprintf "min %s" (toDutchImp -x)
    |  0 -> "nul"
    |  1 -> "een"
    |  2 -> "twee"
    |  3 -> "drie"
    |  4 -> "vier"
    |  5 -> "vijf"
    |  6 -> "zes"
    |  7 -> "zeven"
    |  8 -> "acht"
    |  9 -> "negen"
    | 10 -> "tien"
    | 11 -> "elf"
    | 12 -> "twaalf"
    | 13 -> "dertien"
    | 14 -> "veertien"
    | 15 -> "vijftien"
    | 16 -> "zestien"
    | 17 -> "zeventien"
    | 18 -> "achttien"
    | 19 -> "negentien"
    | Between 20 30 x -> formatTens "twintig" 10 x
    | Between 30 40 x -> formatTens "dertig" 10 x
    | Between 40 50 x -> formatTens "veertig" 10 x
    | Between 50 60 x -> formatTens "vijftig" 10 x
    | Between 60 70 x -> formatTens "zestig" 10 x
    | Between 70 80 x -> formatTens "zeventig" 10 x    
    | Between 80 90 x -> formatTens "tachtig" 10 x
    | Between 90 100 x -> formatTens "negentig" 10 x
    | Between 100 1000 x -> format "honderd" 100 x
    | Between 1000 1000000 x -> format "duizend" 1000 x
    | Between 1000000 1000000000 x -> format "miljoen" 1000000 x
    | _ -> format "miljard" 1000000000 x

let internal tryParseDutchImp (x : string) =
    let rec conv acc total candidate =
        match candidate with
        | ""                       -> Some (total + acc)
        | StartsWith " "         t
        | StartsWith "-"         t
        | StartsWith "EN"        t -> conv                      acc total t
        | StartsWith "ËN"        t -> conv                      acc total t
        | StartsWith "NUL"       t -> conv               (0  + acc) total t     
        | StartsWith "ELF"       t -> conv              (11  + acc) total t   
        | StartsWith "TWAALF"    t -> conv              (12  + acc) total t
        | StartsWith "DERTIEN"   t -> conv              (13  + acc) total t
        | StartsWith "VEERTIEN"  t -> conv              (14  + acc) total t
        | StartsWith "VIJFTIEN"  t -> conv              (15  + acc) total t
        | StartsWith "ZESTIEN"   t -> conv              (16  + acc) total t
        | StartsWith "ZEVENTIEN" t -> conv              (17  + acc) total t
        | StartsWith "ACHTTIEN"  t -> conv              (18  + acc) total t
        | StartsWith "NEGENTIEN" t -> conv              (19  + acc) total t
        | StartsWith "TWINTIG"   t -> conv              (20  + acc) total t
        | StartsWith "DERTIG"    t -> conv              (30  + acc) total t
        | StartsWith "VEERTIG"   t -> conv              (40  + acc) total t
        | StartsWith "VIJFTIG"   t -> conv              (50  + acc) total t
        | StartsWith "ZESTIG"    t -> conv              (60  + acc) total t
        | StartsWith "ZEVENTIG"  t -> conv              (70  + acc) total t
        | StartsWith "TACHTIG"   t -> conv              (80  + acc) total t
        | StartsWith "NEGENTIG"  t -> conv              (90  + acc) total t
        | StartsWith "HONDERD"   t ->
            if acc = 0 then conv 100 total t
            else conv (100 %* acc) total t
        | StartsWith "DUIZEND"   t ->
            if acc = 0 then conv 1000 total t
            else conv 0 (total + 1000 %* acc) t
        | StartsWith "MILJOEN"   t -> 
            if acc = 0 then conv 1000000 total t
            else conv 0 (total + 1000000 %* acc) t
        | StartsWith "MILJARD"   t -> 
            if acc = 0 then conv 1000000000 total t
            else conv 0 (total + 1000000000 %* acc) t
        | StartsWith "EEN"       t
        | StartsWith "ÉÉN"       t -> conv               (1  + acc) total t
        | StartsWith "TWEE"      t -> conv               (2  + acc) total t
        | StartsWith "DRIE"      t -> conv               (3  + acc) total t
        | StartsWith "VIER"      t -> conv               (4  + acc) total t
        | StartsWith "VIJF"      t -> conv               (5  + acc) total t
        | StartsWith "ZES"       t -> conv               (6  + acc) total t
        | StartsWith "ZEVEN"     t -> conv               (7  + acc) total t
        | StartsWith "ACHT"      t -> conv               (8  + acc) total t
        | StartsWith "NEGEN"     t -> conv               (9  + acc) total t
        | StartsWith "TIEN"      t -> conv              (10  + acc) total t    
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "nl")
    match canonicalized with
    | StartsWith "MIN" t -> conv 0 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 0 canonicalized