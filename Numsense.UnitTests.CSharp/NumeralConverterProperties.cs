using FsCheck;
using FsCheck.Xunit;
using Ploeh.Numsense.ObjectOriented;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
            int expected)
        {
            var numeral = p.ToNumeralImp.Invoke(expected);
            int actual;
            var success = p.Sut.TryParse(numeral, out actual);

            Assert.True(success);
            Assert.Equal(expected, actual);
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
                        Ploeh.Numsense.NumeralModule.toEnglish,
                        Ploeh.Numsense.NumeralModule.tryParseEnglish),
                    new ConverterPropertyGroup(
                        new DanishNumeralConverter(),
                        Ploeh.Numsense.NumeralModule.toDanish,
                        Ploeh.Numsense.NumeralModule.tryParseDanish)
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
