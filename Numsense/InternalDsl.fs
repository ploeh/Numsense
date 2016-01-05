module internal Ploeh.Numsense.InternalDsl

let internal (|StartsWith|_|) prefix (candidate : string) =
    if candidate.StartsWith prefix
    then Some (candidate.Substring prefix.Length)
    else None

let internal (|Between|_|) lower upper candidate =
    if lower <= candidate && candidate < upper
    then Some candidate
    else None

let internal (%*) factor x =
    let multiplicand = x % factor
    if multiplicand = 0 
    then x + factor
    else x + (factor * multiplicand) - multiplicand

let internal (|EndsWith|_|) suffix (candidate : string) =
    if candidate.EndsWith suffix
    then Some (candidate.Substring(0,(candidate.Length - suffix.Length)))
    else None

let internal (|Contains|_|) item (candidate : string) =
    if candidate.Contains item 
    then Some (candidate.Substring(0, candidate.IndexOf item), candidate.Substring(item.Length + candidate.IndexOf item, candidate.Length - item.Length - candidate.IndexOf item ))
    else None
