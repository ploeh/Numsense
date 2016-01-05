module internal Ploeh.Numsense.French

open Ploeh.Numsense.InternalDsl

let rec internal toFrenchImp x =

    let format10 prefix factor x =
        let remainder = x % factor
        match remainder with
        | 0  -> prefix 
        | 1  -> sprintf "%s-et-un" prefix
        | _  -> sprintf "%s-%s" prefix (toFrenchImp remainder)

    let format100and1000 suffix factor x =
        let remainder = x % factor
        let prefix = x /factor
        match prefix,remainder with
        | 1, 0 -> sprintf "%s" suffix
        | 1, _ -> sprintf "%s-%s" suffix (toFrenchImp remainder)
        | _, 0 -> sprintf "%s-%s" (toFrenchImp prefix) suffix
        | _, _ -> sprintf "%s-%s-%s" (toFrenchImp prefix) suffix (toFrenchImp remainder)

    let formatOthers suffix factor x =
        let remainder = x % factor
        let prefix =  x /factor
        match prefix, remainder with
        | 1, 0 -> sprintf "%s-%s"  (toFrenchImp prefix) suffix
        | 1, _ -> sprintf "%s-%s-%s"  (toFrenchImp prefix) suffix (toFrenchImp remainder)
        | _, 0 -> sprintf "%s-%ss" (toFrenchImp prefix) suffix
        | _, _ -> sprintf "%s-%ss-%s" (toFrenchImp prefix) suffix (toFrenchImp remainder)

    let without80And100Plural x =
        match x with
        |  x when x < 0 -> sprintf "moins %s" (toFrenchImp -x)
        |  0 -> "zéro"
        |  1 -> "un"
        |  2 -> "deux"
        |  3 -> "trois"
        |  4 -> "quatre"
        |  5 -> "cinq"
        |  6 -> "six"
        |  7 -> "sept"
        |  8 -> "huit"
        |  9 -> "neuf"
        | 10 -> "dix"
        | 11 -> "onze"
        | 12 -> "douze"
        | 13 -> "treize"
        | 14 -> "quatorze"
        | 15 -> "quinze"
        | 16 -> "seize"
        | 71 -> "soixante-et-onze"
        | 81 -> "quatre-vingt-un"
        | Between 10 20 x -> format10 "dix" 10 x
        | Between 20 30 x -> format10 "vingt" 10 x
        | Between 30 40 x -> format10 "trente" 10 x
        | Between 40 50 x -> format10 "quarante" 10 x
        | Between 50 60 x -> format10 "cinquante" 10 x
        | Between 60 70 x -> format10 "soixante" 10 x
        | Between 70 80 x -> format10 "soixante" 20 x
        | Between 80 90 x -> format10 "quatre-vingt" 10 x
        | Between 90 100 x -> format10 "quatre-vingt" 20 x
        | Between 100 1000 x -> format100and1000 "cent" 100 x
        | Between 1000 1000000 x -> format100and1000 "mille" 1000 x
        | Between 1000000 1000000000 x -> formatOthers "million" 1000000 x
        | _ -> formatOthers "milliard" 1000000000 x

    let pluralize100 x =
        let check100Prefix (prefix :string) (item :string) (suffix :string) =
            match prefix with
            | EndsWith "deux"   p 
            | EndsWith "trois"  p
            | EndsWith "quatre" p
            | EndsWith "cinq"   p
            | EndsWith "six"    p
            | EndsWith "sept"   p
            | EndsWith "huit"   p
            | EndsWith "neuf"   p
            | EndsWith "dix"    p
            | EndsWith "ze"     p -> prefix + item.Replace("cent", "cents") + suffix // works for 11, 12, 13, 14, 15, 16
            | _                   -> prefix + item + suffix
        let pluralize100' (s : string) =
            match s with
            | EndsWith "-cent" p -> check100Prefix p "-cent" ""
            | _ -> s

        let pluralize100mille (s : string) =
            match s with
            | Contains "-cent-mille"  (p,e) -> check100Prefix p "-cent-mille" e
            | _-> s

        let pluralize100millions (s: string) =
            match s with
            | Contains "-cent-millions" (p,e) -> check100Prefix p "-cent-millions" e
            | _ -> s
            
        let pluralize100milliards (s :string) =
            match s with
            | Contains "-cent-milliards" (p,e) -> check100Prefix p "-cent-milliards" e
            | _ -> s
        x
        |> pluralize100'
        |> pluralize100mille
        |> pluralize100millions
        |> pluralize100milliards

    
    let pluralize80 (x :string) =
        let pluralize80' (s : string) =
            match s.EndsWith "quatre-vingt" with
            | true ->  s + "s"
            | false -> s
        let pluralize80millions (s : string) =
            match s.Contains "quatre-vingt-million" with
            | true ->  s.Replace("quatre-vingt-million", "quatre-vingts-million")
            | false -> s
        let pluralize80milliard (s : string) =
            match s.Contains "quatre-vingt-milliard" with
            | true ->  s.Replace("quatre-vingt-milliard", "quatre-vingts-milliard")
            | false -> s
        x 
        |> pluralize80'
        |> pluralize80millions
        |> pluralize80millions
    x
    |> without80And100Plural
    |> pluralize80
    |> pluralize100

let internal tryParseFrenchImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                           -> Some acc
        // Plural 
        | StartsWith "IX"            t
        | StartsWith "SIX"           t -> conv          (6  + acc) t
        | StartsWith "EPT"           t
        | StartsWith "SEPT"          t -> conv          (7  + acc) t
        | StartsWith "EIZE"          t
        | StartsWith "SEIZE"         t -> conv         (16  + acc) t
        | StartsWith "OIXANTE"       t
        | StartsWith "SOIXANTE"      t -> conv         (60  + acc) t
        
        | StartsWith "-"             t
        | StartsWith "ET"            t -> conv                acc  t
        | StartsWith "ZÉRO"          t 
        | StartsWith "ZERO"          t -> conv          (0  + acc) t
        | StartsWith "UN"            t -> conv          (1  + acc) t
        | StartsWith "DEUX"          t -> conv          (2  + acc) t
        | StartsWith "TROIS"         t -> conv          (3  + acc) t
        | StartsWith "QUATREVINGTS"  t 
        | StartsWith "QUATRE-VINGTS" t     
        | StartsWith "QUATREVINGT"   t 
        | StartsWith "QUATRE-VINGT"  t -> conv          (80  + acc) t  
        | StartsWith "QUATRE"        t -> conv          (4  + acc) t
        | StartsWith "CINQUANTE"     t -> conv          (50  + acc) t
        | StartsWith "CINQ"          t -> conv          (5  + acc) t
        | StartsWith "HUIT"          t -> conv          (8  + acc) t
        | StartsWith "NEUF"          t -> conv          (9  + acc) t
        | StartsWith "DIX"           t -> conv         (10  + acc) t
        | StartsWith "ONZE"          t -> conv         (11  + acc) t
        | StartsWith "DOUZE"         t -> conv         (12  + acc) t
        | StartsWith "TREIZE"        t -> conv         (13  + acc) t
        | StartsWith "QUATORZE"      t -> conv         (14  + acc) t
        | StartsWith "QUINZE"        t -> conv         (15  + acc) t
        | StartsWith "VINGT"         t -> conv         (20  + acc) t
        | StartsWith "TRENTE"        t -> conv         (30  + acc) t
        | StartsWith "QUARANTE"      t -> conv         (40  + acc) t
        | StartsWith "CENTS"         t
        | StartsWith "CENT"          t -> conv        (100 %* acc) t
        | StartsWith "MILLE"         t -> conv       (1000 %* acc) t
        | StartsWith "MILLIONS"      t
        | StartsWith "MILLION"       t -> conv    (1000000 %* acc) t
        | StartsWith "MILLIARDS"     t
        | StartsWith "MILLIARD"      t -> conv (1000000000 %* acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "en")
    match canonicalized with
    | StartsWith "MOINS" t -> conv 0 (t.Trim ()) |> Option.map ((*)-1)
    | _ -> conv 0 canonicalized
