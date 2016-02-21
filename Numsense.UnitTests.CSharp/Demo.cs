using Ploeh.Numsense.ObjectOriented;
using Xunit;

namespace Ploeh.Numsense.UnitTests
{
    /// <summary>
    /// Demo code as tests.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The purpose of this file is exclusively as an introduction to Numsense.
    /// While it contains tests, these tests don't cover functionality not
    /// covered elsewhere, by other tests.
    /// </para>
    /// <para>
    /// The code in this file can serve as a quick introduction to Numsense.
    /// Additionally, some code snippet from this file are used for
    /// documentation purposes.
    /// </para>
    /// </remarks>
    public class Demo
    {
        [Fact]
        public void ConvertIntegerToEnglish()
        {
            var englishNumeral = Numeral.English.ToNumeral(42);
            Assert.Equal("forty-two", englishNumeral);
        }

        [Fact]
        public void ParseEnglishNumeralToInteger()
        {
            int i;
            var success = Numeral.English.TryParse(
                "one-thousand-three-hundred-thirty-seven",
                out i);

            Assert.True(success);
            Assert.Equal(1337, i);
        }

        [Fact]
        public void ConvertIntegerToDanish()
        {
            var danishNumeral = Numeral.Danish.ToNumeral(9);
            Assert.Equal("ni", danishNumeral);
        }

        [Fact]
        public void ConvertIntegerToBulgarian()
        {
            var bulgarianNumeral = Numeral.Bulgarian.ToNumeral(21);
            Assert.Equal("двайсет-и-едно", bulgarianNumeral);
        }

        [Fact]
        public void ParseBulgarianNumeralToInteger()
        {
            int i;
            var success = Numeral.Bulgarian.TryParse(
                "четиресет и две",
                out i);

            Assert.True(success);
            Assert.Equal(42, i);
        }
    }
}
