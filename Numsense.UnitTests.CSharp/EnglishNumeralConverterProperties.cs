using FsCheck;
using FsCheck.Xunit;
using Ploeh.Numsense.ObjectOriented;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Ploeh.Numsense.UnitTests
{
    public class EnglishNumeralConverterProperties
    {
        [Property(QuietOnSuccess = true)]
        public void ToNumeralReturnsCorrectResult(int i)
        {
            INumeralConverter sut = new EnglishNumeralConverter();

            var actual = sut.ToNumeral(i);

            var expected = Ploeh.Numsense.Numeral.toEnglish.Invoke(i);
            Assert.Equal(expected, actual);
        }

        [Property(QuietOnSuccess = true)]
        public void TryParseProperNumeralReturnsCorrectResult(int expected)
        {
            INumeralConverter sut = new EnglishNumeralConverter();

            var numeral = Ploeh.Numsense.Numeral.toEnglish.Invoke(expected);
            int actual;
            var success = sut.TryParse(numeral, out actual);

            Assert.True(success);
            Assert.Equal(expected, actual);
        }

        [Property(QuietOnSuccess = true)]
        public Property TryParseGarbageTextReturnsCorrectResult()
        {
            var garbage = Arb.Default.String().Filter(s =>
                s == null ||
                Ploeh.Numsense.Numeral.tryParseEnglish.Invoke(s) == null);
            return Prop.ForAll(
                garbage,
                s =>
                {
                    INumeralConverter sut = new EnglishNumeralConverter();

                    int actual;
                    var success = sut.TryParse(s, out actual);

                    Assert.False(success);
                    Assert.Equal(default(int), actual);
                });
        }
    }
}
