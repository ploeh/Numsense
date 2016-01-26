using FsCheck;
using FsCheck.Xunit;
using Ploeh.Numsense.ObjectOriented;
using Xunit;
using Microsoft.FSharp.Core;

namespace Ploeh.Numsense.UnitTests
{
    public class NumeralConverterProperties
    {
        [ConverterProperty]
        public void ToNumeralReturnsCorrectResult(
            ConverterPropertyGroup p,
            int i)
        {
            var actual = p.Sut.ToNumeral(i);

            var expected = p.ToNumeralImp.Invoke(i);
            Assert.Equal(expected, actual);
        }

        [ConverterProperty]
        public void TryParseProperNumeralReturnsCorrectResult(
            ConverterPropertyGroup p,
            int i)
        {
            var numeral = p.ToNumeralImp.Invoke(i);

            int actual;
            var success = p.Sut.TryParse(numeral, out actual);

            var expected = p.TryParseImp.Invoke(numeral);
            Assert.True(success);
            Assert.Equal(expected, FSharpOption<int>.Some(actual));
        }

        [ConverterProperty]
        public Property TryParseGarbageTextReturnsCorrectResult(
            ConverterPropertyGroup p)
        {
            var garbage = Arb.Default.String().Filter(s =>
                s == null ||
                p.TryParseImp.Invoke(s) == null);
            return Prop.ForAll(
                garbage,
                s =>
                {
                    int actual;
                    var success = p.Sut.TryParse(s, out actual);

                    Assert.False(success);
                    Assert.Equal(default(int), actual);
                });
        }

        public class ConverterPropertyAttribute : PropertyAttribute
        {
            public ConverterPropertyAttribute()
            {
                QuietOnSuccess = true;
                Arbitrary = new[] { typeof(ConverterPropertyArb) };
            }
        }

        public static class ConverterPropertyArb
        {
            public static Arbitrary<ConverterPropertyGroup> Converter()
            {
                return Gen
                    .Elements(
                    new ConverterPropertyGroup(
                        new EnglishNumeralConverter(),
                        NumeralModule.toEnglish,
                        NumeralModule.tryParseEnglish),
                    new ConverterPropertyGroup(
                        new DanishNumeralConverter(),
                        NumeralModule.toDanish,
                        NumeralModule.tryParseDanish),
                    new ConverterPropertyGroup(
                        new GermanNumeralConverter(),
                        NumeralModule.toGerman,
                        NumeralModule.tryParseGerman),
                    new ConverterPropertyGroup(
                        new PolishNumeralConverter(),
                        NumeralModule.toPolish,
                        NumeralModule.tryParsePolish),
                    new ConverterPropertyGroup(
                        new DutchNumeralConverter(),
                        NumeralModule.toDutch,
                        NumeralModule.tryParseDutch)
                    )
                    .ToArbitrary();
            }
        }

        public class ConverterPropertyGroup
        {
            public readonly INumeralConverter Sut;
            public readonly FSharpFunc<int, string> ToNumeralImp;
            public readonly FSharpFunc<string, FSharpOption<int>> TryParseImp;

            public ConverterPropertyGroup(
                INumeralConverter sut,
                FSharpFunc<int, string> toNumeralImp,
                FSharpFunc<string, FSharpOption<int>> tryParseImp)
            {
                this.Sut = sut;
                this.ToNumeralImp = toNumeralImp;
                this.TryParseImp = tryParseImp;
            }

            public override string ToString()
            {
                return this.Sut.GetType().Name;
            }
        }
    }
}
