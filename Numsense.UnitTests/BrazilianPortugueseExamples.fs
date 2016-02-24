module Ploeh.Numsense.BrazilianPortugueseExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(  "zero", 0)>]
[<InlineData(    "um", 1)>]
[<InlineData(  "dois", 2)>]
[<InlineData(  "três", 3)>]
[<InlineData("quatro", 4)>]
[<InlineData( "cinco", 5)>]
[<InlineData(  "seis", 6)>]
[<InlineData(  "sete", 7)>]
[<InlineData(  "oito", 8)>]
[<InlineData(  "nove", 9)>]
let ``tryParseBrazilian returns correct result`` (portuguese, expected) =
    let actual = Numeral.tryParseBrazilian portuguese
    Some expected =! actual
