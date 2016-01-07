using Ploeh.Numsense.ObjectOriented;
using Xunit;

namespace Ploeh.Numsense.UnitTests
{
    public class NumeralTests
    {
        [Fact]
        public void EnglishIsCorrect()
        {
            var actual = Numeral.English;
            Assert.IsAssignableFrom<EnglishNumeralConverter>(actual);
        }

        [Fact]
        public void EnglishIsSingleton()
        {
            var expected = Numeral.English;
            var actual = Numeral.English;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void DanishIsCorrect()
        {
            var actual = Numeral.Danish;
            Assert.IsAssignableFrom<DanishNumeralConverter>(actual);
        }

        [Fact]
        public void DanishIsSingleton()
        {
            var expected = Numeral.Danish;
            var actual = Numeral.Danish;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void FarsiIsCorrect()
        {
            var actual = Numeral.Farsi;
            Assert.IsAssignableFrom<FarsiNumeralConverter>(actual);
        }

        [Fact]
        public void FarsiIsSingleton()
        {
            var expected = Numeral.Farsi;
            var actual = Numeral.Farsi;
            Assert.Same(expected, actual);
        }
    }
}
