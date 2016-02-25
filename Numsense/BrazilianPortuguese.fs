module internal Ploeh.Numsense.BrazilianPortuguese

open Ploeh.Numsense.InternalDsl

let internal tryParseBrazilianImp (x : string) =
    let rec conv acc candidate =
        match candidate with
        | ""                          -> Some acc
        | StartsWith " "            t
        | StartsWith "E"            t -> conv acc t
        | "ZERO"                      -> Some   (0  + acc)
        | StartsWith "CEM"          t -> Some (100  + acc)
        | StartsWith "CENTOS"       t -> conv (100 %* acc) t
        | StartsWith "CENTO"        t -> conv (100  + acc) t
        | StartsWith "DUZENTOS"     t -> conv (200  + acc) t
        | StartsWith "TREZENTOS"    t -> conv (300  + acc) t
        | StartsWith "QUINHENTOS"   t -> conv (500  + acc) t
        | StartsWith "VINTE"        t -> conv  (20  + acc) t
        | StartsWith "TRINTA"       t -> conv  (30  + acc) t
        | StartsWith "QUARENTA"     t -> conv  (40  + acc) t
        | StartsWith "CINQUENTA"    t -> conv  (50  + acc) t
        | StartsWith "CINQÜENTA"    t -> conv  (50  + acc) t
        | StartsWith "SESSENTA"     t -> conv  (60  + acc) t
        | StartsWith "SETENTA"      t -> conv  (70  + acc) t
        | StartsWith "OITENTA"      t -> conv  (80  + acc) t
        | StartsWith "NOVENTA"      t -> conv  (90  + acc) t
        | "DEZ"                       -> Some  (10  + acc)
        | "ONZE"                      -> Some  (11  + acc)
        | "DOZE"                      -> Some  (12  + acc)
        | "TREZE"                     -> Some  (13  + acc)
        | "CATORZE"                   -> Some  (14  + acc)
        | "QUATORZE"                  -> Some  (14  + acc)
        | "QUINZE"                    -> Some  (15  + acc)
        | "DEZESSEIS"                 -> Some  (16  + acc)
        | "DEZESSETE"                 -> Some  (17  + acc)
        | "DEZOITO"                   -> Some  (18  + acc)
        | "DEZENOVE"                  -> Some  (19  + acc)
        | "UM"                        -> Some   (1  + acc)
        | "DOIS"                      -> Some   (2  + acc)
        | "TRÊS"                      -> Some   (3  + acc)
        | StartsWith "QUATRO"       t -> conv   (4  + acc) t
        | "CINCO"                     -> Some   (5  + acc)
        | StartsWith "SEIS"         t -> conv   (6  + acc) t
        | StartsWith "SETE"         t -> conv   (7  + acc) t
        | StartsWith "OITO"         t -> conv   (8  + acc) t
        | StartsWith "NOVE"         t -> conv   (9  + acc) t
        | _ -> None

    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "pt-BR")
    conv 0 canonicalized
