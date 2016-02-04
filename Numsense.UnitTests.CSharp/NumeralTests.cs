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

        public void PolishIsCorrect()
        {
            var actual = Numeral.Polish;
            Assert.IsAssignableFrom<PolishNumeralConverter>(actual);
        }

        [Fact]
        public void PolishIsSingleton()
        {
            var expected = Numeral.Polish;
            var actual = Numeral.Polish;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void DutchIsCorrect()
        {
            var actual = Numeral.Dutch;
            Assert.IsAssignableFrom<DutchNumeralConverter>(actual);
        }

        [Fact]
        public void DutchIsSingleton()
        {
            var expected = Numeral.Dutch;
            var actual = Numeral.Dutch;
            Assert.Same(expected, actual);
        }
        
        [Fact]
        public void RussianIsCorrect()
        {
            var actual = Numeral.Russian;
            Assert.IsAssignableFrom<RussianNumeralConverter>(actual);
        }

        [Fact]
        public void RussianIsSingleton()
        {
            var expected = Numeral.Russian;
            var actual = Numeral.Russian;
            Assert.Same(expected, actual);
        }
        
        [Fact]
        public void TraditionalChineseIsCorrect()
        {
            var actual = Numeral.TraditionalChinese;
            Assert.IsAssignableFrom<TraditionalChineseNumeralConverter>(actual);
        }

        [Fact]
        public void TraditionalChineseIsSingleton()
        {
            var expected = Numeral.TraditionalChinese;
            var actual = Numeral.TraditionalChinese;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void TraditionalFinancialChineseIsCorrect()
        {
            var actual = Numeral.TraditionalFinancialChinese;
            Assert.IsAssignableFrom
                <TraditionalFinancialChineseNumeralConverter>(actual);
        }

        [Fact]
        public void TraditionalFinancialChineseIsSingleton()
        {
            var expected = Numeral.TraditionalFinancialChinese;
            var actual = Numeral.TraditionalFinancialChinese;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void SimplifiedChineseIsCorrect()
        {
            var actual = Numeral.SimplifiedChinese;
            Assert.IsAssignableFrom<SimplifiedChineseNumeralConverter>(actual);
        }

        [Fact]
        public void SimplifiedChineseIsSingleton()
        {
            var expected = Numeral.SimplifiedChinese;
            var actual = Numeral.SimplifiedChinese;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void SimplifiedFinancialChineseIsCorrect()
        {
            var actual = Numeral.SimplifiedFinancialChinese;
            Assert.IsAssignableFrom
                <SimplifiedFinancialChineseNumeralConverter>(actual);
        }

        [Fact]
        public void SimplifiedFinancialChineseIsSingleton()
        {
            var expected = Numeral.SimplifiedFinancialChinese;
            var actual = Numeral.SimplifiedFinancialChinese;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void CatalanIsCorrect()
        {
            var actual = Numeral.Catalan;
            Assert.IsAssignableFrom<CatalanNumeralConverter>(actual);
        }

        [Fact]
        public void CatalanIsSingleton()
        {
            var expected = Numeral.Catalan;
            var actual = Numeral.Catalan;
            Assert.Same(expected, actual);
        }
    }
}
