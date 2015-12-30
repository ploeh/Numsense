# Numsense

A .NET library for parsing natural-language numerals ("forty-two") to integers, and converting the other way as well. While written in F#, it can be used from both F# and C#:

### F#:

```F#
> let englishNumeral = Numeral.toEnglish 42;;
val englishNumeral : string = "forty-two"

> let i = Numeral.tryParseEnglish "one-thousand-three-hundred-thirty-seven";;
val i : int option = Some 1337
```

### C#:

```C#
var englishNumeral = Numeral.English.ToNumeral(42);
// englishNumeral is "forty-two"

int i;
var success = Numeral.English.TryParse(
    "one-thousand-three-hundred-thirty-seven",
    out i);
// success is true, and i is 1337
```

Other languages than English are supported as well:

```F#
> let danishNumeral = Numeral.toDanish 9;;
val danishNumeral : string = "ni"
```

If you don't see your favourite language, please consider submitting a pull request.

## Versioning

Numsense follows [Semantic Versioning 2.0.0](http://semver.org/spec/v2.0.0.html).