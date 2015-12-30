// The purpose of this file is exclusively as an introduction to Numsense.
// While it contains tests, these tests don't cover functionality not covered
// elsewhere, by other tests.

// The code in this file can serve as a quick introduction to Numsense.
// Additionally, some code snippet from this file are used for documentation
// purposes.

module Ploeh.Numsense.Demo

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Convert integer to English`` () =
    let englishNumeral = Numeral.toEnglish 42
    "forty-two" =! englishNumeral

[<Fact>]
let ``Parse English numeral to integer`` () =
    let i = Numeral.tryParseEnglish "one-thousand-three-hundred-thirty-seven"
    Some 1337 =! i

[<Fact>]
let ``Convert integer to Danish`` () =
    let danishNumeral = Numeral.toDanish 9
    "ni" =! danishNumeral