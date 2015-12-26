module Ploeh.Numsense.EnglishExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(     "zero", 0)>]
[<InlineData(    " zero", 0)>]
[<InlineData(    "zero ", 0)>]
[<InlineData( "  zero  ", 0)>]
[<InlineData(     "Zero", 0)>]
[<InlineData(     "ZERO", 0)>]
[<InlineData(  " zERo\t", 0)>]
[<InlineData(      "one", 1)>]
[<InlineData(      "ONE", 1)>]
[<InlineData(      "two", 2)>]
[<InlineData(     " two", 2)>]
[<InlineData(    "three", 3)>]
[<InlineData(  "three  ", 3)>]
[<InlineData(     "four", 4)>]
[<InlineData(  "  four ", 4)>]
[<InlineData(     "five", 5)>]
[<InlineData(     "FivE", 5)>]
[<InlineData(      "six", 6)>]
[<InlineData(  "  SIX  ", 6)>]
[<InlineData(    "seven", 7)>]
[<InlineData("    seVen", 7)>]
[<InlineData(    "eight", 8)>]
[<InlineData(    "eIGHT", 8)>]
[<InlineData(     "nine", 9)>]
[<InlineData(   "NiNe  ", 9)>]
let ``ofEnglish returns correct result`` (english : string, expected : int) =
    let actual = Numeral.ofEnglish english
    expected =! actual