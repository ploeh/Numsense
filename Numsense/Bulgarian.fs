module internal Ploeh.Numsense.Bulgarian

open Ploeh.Numsense.InternalDsl

type private Gender = | Masculine | Feminine | Neuter

let internal toBulgarianImp x = 

    let rec toBulgarian x gender = 

        let simplify prefix factor x = 
            let remainder = x % factor
            let rec nonTeens n = 
                let d = if n % 1000 < 20 then 0 else 1
                if n > 0 then d + nonTeens (n / 1000) else 0
            let rec numDigits n =
                let d = if n % 10 <> 0 then 1 else 0
                if n > 0 then d + numDigits (n / 10) else 0
            match remainder with
            | 0 -> prefix
            | r when nonTeens r = 0 || numDigits r = 1 ->
                sprintf "%s-и-%s" prefix (toBulgarian r gender) // add "-and-"
            | _ -> sprintf "%s-%s" prefix (toBulgarian remainder gender)
        
        let format suffix factor x = 
            let prefix = sprintf "%s%s" (toBulgarian (x / factor) gender) suffix
            simplify prefix factor x
        
        let formatTens x = 
            let tens = x / 10
            match x with
            | 10 -> "десет"
            | 11 -> "единайсет"
            | 12 -> "дванайсет"
            | 13 -> "тринайсет"
            | 14 -> "четиринайсет"
            | 15 -> "петнайсет"
            | 16 -> "шестнайсет"
            | 17 -> "седемнайсет"
            | 18 -> "осемнайсет"
            | 19 -> "деветнайсет"
            | _ -> 
                match tens with
                | 2 -> simplify "двайсет" 10 x
                | 3 -> simplify "трийсет" 10 x
                | 4 -> simplify "четиресет" 10 x
                | 6 -> simplify "шейсет" 10 x
                | _ -> simplify (format "десет" 1 tens) 10 x
        
        let formatHundreds x = 
            let quotient = x / 100
            let quotientText = 
                match quotient with
                | 1 -> "сто"
                | 2 -> "двеста"
                | 3 -> "триста"
                | _ -> sprintf "%sстотин" (toBulgarian quotient Neuter)
            simplify quotientText 100 x
        
        let formatThousands x = 
            let quotient = x / 1000
            let quotientText = 
                match quotient with
                | 1 -> "хиляда"
                | _ -> sprintf "%s-хиляди" (toBulgarian quotient Feminine)
            simplify quotientText 1000 x
        
        let formatMillions x = 
            let quotient = x / 1000000
            let quotientText = 
                match quotient with
                | 1 -> "един-милион"
                | 2 -> "два-милиона"
                | _ -> sprintf "%s-милиона" (toBulgarian quotient Masculine)
            simplify quotientText 1000000 x
        
        let formatBillions x = 
            let quotient = x / 1000000000
            let quotientText = 
                match quotient with
                | 1 -> "един-милиард"
                | 2 -> "два-милиарда"
                | _ -> sprintf "%s-милиарда" (toBulgarian quotient Masculine)
            simplify quotientText 1000000000 x
        
        match x with
        | x when x < 0 -> sprintf "минус %s" (toBulgarian -x gender)
        | 0 -> "нула"
        | 1 -> match gender with
               | Masculine -> "един"
               | Feminine -> "една"
               | Neuter -> "едно"
        | 2 -> match gender with
               | Masculine -> "два"
               | Feminine -> "две"
               | Neuter -> "две"
        | 3 -> "три"
        | 4 -> "четири"
        | 5 -> "пет"
        | 6 -> "шест"
        | 7 -> "седем"
        | 8 -> "осем"
        | 9 -> "девет"
        | Between 10 100 x -> formatTens x
        | Between 100 1000 x -> formatHundreds x
        | Between 1000 1000000 x -> formatThousands x
        | Between 1000000 1000000000 x -> formatMillions x
        | _ -> formatBillions x
    toBulgarian x Neuter

let internal tryParseBulgarianImp (x : string) = 
    let rec conv acc candidate = 
        match candidate with
        | "" -> Some acc
        | StartsWith " " t | StartsWith "-" t | StartsWith "И" t -> conv acc t
        | StartsWith "НУЛА" t -> conv (0 + acc) t
        | StartsWith "ЕДИНАЙСЕТ" t -> conv (11 + acc) t
        | StartsWith "ЕДИНАДЕСЕТ" t -> conv (11 + acc) t
        | StartsWith "ЕДИН" t -> conv (1 + acc) t
        | StartsWith "ЕДНА" t -> conv (1 + acc) t
        | StartsWith "ЕДНО" t -> conv (1 + acc) t
        | StartsWith "ДВЕСТА" t -> conv (200 + acc) t
        | StartsWith "ДВЕ" t -> conv (2 + acc) t
        | StartsWith "ДВАЙСЕТ" t -> conv (20 + acc) t
        | StartsWith "ДВАДЕСЕТ" t -> conv (20 + acc) t
        | StartsWith "ДВА" t -> conv (2 + acc) t
        | StartsWith "ТРИЙСЕТ" t -> conv (30 + acc) t
        | StartsWith "ТРИДЕСЕТ" t -> conv (30 + acc) t
        | StartsWith "ТРИСТА" t -> conv (300 + acc) t
        | StartsWith "ТРИ" t -> conv (3 + acc) t
        | StartsWith "ЧЕТИРЕСЕТ" t -> conv (40 + acc) t
        | StartsWith "ЧЕТИРИДЕСЕТ" t -> conv (40 + acc) t
        | StartsWith "ЧЕТИРИ" t -> conv (4 + acc) t
        | StartsWith "ПЕТДЕСЕТ" t -> conv (50 + acc) t
        | StartsWith "ПЕТ" t -> conv (5 + acc) t
        | StartsWith "ШЕЙСЕТ" t -> conv (60 + acc) t
        | StartsWith "ШЕСТДЕСЕТ" t -> conv (60 + acc) t
        | StartsWith "ШЕСТ" t -> conv (6 + acc) t
        | StartsWith "СЕДЕМДЕСЕТ" t -> conv (70 + acc) t
        | StartsWith "СЕДЕМ" t -> conv (7 + acc) t
        | StartsWith "ОСЕМДЕСЕТ" t -> conv (80 + acc) t
        | StartsWith "ОСЕМ" t -> conv (8 + acc) t
        | StartsWith "ДЕВЕТДЕСЕТ" t -> conv (90 + acc) t
        | StartsWith "ДЕВЕТ" t -> conv (9 + acc) t
        | StartsWith "ДЕСЕТ" t -> conv (10 + acc) t
        | StartsWith "НАЙСЕТ" t -> conv (10 + acc) t
        | StartsWith "НАДЕСЕТ" t -> conv (10 + acc) t
        | StartsWith "СТОТИН" t -> conv (100 %* acc) t
        | StartsWith "СТО" t -> conv (100 + acc) t
        | StartsWith "ХИЛЯДА" t -> conv (1000 + acc) t
        | StartsWith "ХИЛЯДИ" t -> conv (1000 %* acc) t
        | StartsWith "МИЛИОНА" t -> conv (1000000 %* acc) t
        | StartsWith "МИЛИАРДА" t -> conv (1000000000 %* acc) t
        | StartsWith "МИЛИОН" t -> 
            conv (if acc = 0 then 1000000 else 1000000 %* acc) t
        | StartsWith "МИЛИАРД" t -> 
            conv (if acc = 0 then 1000000000 else 1000000000 %* acc) t
        | _ -> None
    
    let canonicalized = x.Trim().ToUpper(System.Globalization.CultureInfo "bg-BG")
    match canonicalized with
    | StartsWith "МИНУС" t -> conv 0 (t.Trim()) |> Option.map ((*) -1)
    | _ -> conv 0 canonicalized
