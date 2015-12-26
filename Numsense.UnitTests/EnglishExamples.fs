module Ploeh.Numsense.EnglishExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData( "zero", 0)>]
[<InlineData(  "one", 1)>]
[<InlineData(  "two", 2)>]
[<InlineData("three", 3)>]
[<InlineData( "four", 4)>]
[<InlineData( "five", 5)>]
[<InlineData(  "six", 6)>]
[<InlineData("seven", 7)>]
[<InlineData("eight", 8)>]
[<InlineData( "nine", 9)>]
let ``ofEnglish returns correct result`` (english : string, expected : int) =
    let actual = Numeral.ofEnglish english
    expected =! actual