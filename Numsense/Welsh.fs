module internal Ploeh.Numsense.Welsh

open Ploeh.Numsense.InternalDsl


let rec internal toWelshImp x =

    let simplify prefix factor x =
        let remainder = x % factor
        if remainder = 0
        then prefix
        else sprintf "%s-%s" prefix (toWelshImp (remainder))

    let format suffix factor x =
        let prefix = sprintf "%s%s" (toWelshImp (x / factor)) suffix
        simplify prefix factor x

    // This is a simplified implementation : the actual form used can depend on the context,
    // see http://www.omniglot.com/language/numbers/welsh.htm and https://en.wikipedia.org/wiki/Welsh_numerals
    // It may be that someone with better Welsh could improve this...

    match x with
    |  x when x < 0 -> sprintf "minus %s" (toWelshImp -x)
    // units (male)
    |  0 -> "dim"
    |  1 -> "un"
    |  2 -> "dau"       // or "dwy" (female)
    |  3 -> "tri"       // or "tair" (female)
    |  4 -> "pedwar"    // or "pedair" (female)
    |  5 -> "pump"      // can be shortened to "pum"
    |  6 -> "chwech"    // can be shortened to "chwe"
    |  7 -> "saith"
    |  8 -> "wyth"
    |  9 -> "naw"
    | 10 -> "deg"
    // tens are "n-deg" except 2x (mutation after vowel) and 5x & 6x (shortened form)
    | Between 20 30 x  -> simplify "dau-ddeg" 10 x
    | Between 50 60 x  -> simplify "pum-deg"  10 x
    | Between 60 70 x  -> simplify "chwe-deg" 10 x
    | Between 10 100 x -> format   "-deg"     10 x
    // hundreds are "n-cant" except for 1xx (no "un"), 2xx & 3xx (mutation after vowel) and 5xx & 6xx (shortened form)
    | Between 100 200 x  -> simplify "cant"       100 x
    | Between 200 300 x  -> simplify "dau-gant"   100 x
    | Between 300 400 x  -> simplify "tri-chant"  100 x
    | Between 500 600 x  -> simplify "pum-cant"   100 x
    | Between 600 700 x  -> simplify "chwe-chant" 100 x
    | Between 100 1000 x -> format   "-cant"      100 x
    // thousands
    | Between 1000 1000000 x -> format "-mil" 1000 x
    // millions
    | Between 1000000 1000000000 x -> format "-miliwn" 1000000 x
    // billions
    | _ -> format "-biliwn" 1000000000 x


let internal tryParseWelshImp (x : string) =

    let rec conv acc candidate =

        let conv' x acc t =
            conv (if acc = 0 then x else x %* acc) t

        match candidate with
        | ""                       -> Some acc
        | StartsWith "-"         t
        // units                 
        | StartsWith "DIM"       t -> conv          (0  + acc) t
        | StartsWith "UN"        t -> conv          (1  + acc) t
        | StartsWith "DAU"       t
        | StartsWith "DWY"       t -> conv          (2  + acc) t     // female
        | StartsWith "TRI"       t
        | StartsWith "TAIR"      t -> conv          (3  + acc) t     // female
        | StartsWith "PEDWAR"    t
        | StartsWith "PEDAIR"    t -> conv          (4  + acc) t     // female
        | StartsWith "PUMP"      t
        | StartsWith "PUM"       t -> conv          (5  + acc) t     // shortened
        | StartsWith "CHWECHANT" t -> conv         (600 + acc) t     // mutated
        | StartsWith "CHWECH"    t
        | StartsWith "CHWE"      t -> conv          (6  + acc) t     // shortened
        | StartsWith "SAITH"     t -> conv          (7  + acc) t
        | StartsWith "WYTH"      t -> conv          (8  + acc) t
        | StartsWith "NAW"       t -> conv          (9  + acc) t
        | StartsWith "DEG"       t
        | StartsWith "DDEG"      t -> conv'             10 acc t     // mutated
        | StartsWith "CANT"      t
        | StartsWith "GANT"      t                                   // mutated
        | StartsWith "CHANT"     t -> conv'            100 acc t     // mutated
        | StartsWith "MILIWN"    t -> conv'        1000000 acc t
        | StartsWith "MIL"       t -> conv'           1000 acc t
        | StartsWith "BILIWN"    t -> conv'     1000000000 acc t
        | _                        -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "cy")

    match canonicalized with
    | StartsWith "MINUS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _                    -> conv 0 canonicalized
