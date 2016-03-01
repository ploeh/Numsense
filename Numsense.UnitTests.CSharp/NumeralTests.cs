﻿using Ploeh.Numsense.ObjectOriented;
using Xunit;

namespace Ploeh.Numsense.UnitTests
{
    public class NumeralTests
    {
        [Fact]
        public void BulgarianIsCorrect()
        {
            var actual = Numeral.Bulgarian;
            Assert.IsAssignableFrom<BulgarianNumeralConverter>(actual);
        }

        [Fact]
        public void BulgarianIsSingleton()
        {
            var expected = Numeral.Bulgarian;
            var actual = Numeral.Bulgarian;
            Assert.Same(expected, actual);
        }

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
        public void PortugueseIsCorrect()
        {
            var actual = Numeral.Portuguese;
            Assert.IsAssignableFrom<PortugueseNumeralConverter>(actual);
        }

        [Fact]
        public void PortugueseIsSingleton()
        {
            var expected = Numeral.Portuguese;
            var actual = Numeral.Portuguese;
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
        public void SpanishIsCorrect()
        {
            var actual = Numeral.Spanish;
            Assert.IsAssignableFrom<SpanishNumeralConverter>(actual);
        }

        [Fact]
        public void SpanishIsSingleton()
        {
            var expected = Numeral.Spanish;
            var actual = Numeral.Spanish;
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

        [Fact]
        public void SwedishIsCorrect()
        {
            var actual = Numeral.Swedish;
            Assert.IsAssignableFrom<SwedishNumeralConverter>(actual);
        }

        [Fact]
        public void SwedishIsSingleton()
        {
            var expected = Numeral.Swedish;
            var actual = Numeral.Swedish;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void RomanianIsCorrect()
        {
            var actual = Numeral.Romanian;
            Assert.IsAssignableFrom<RomanianNumeralConverter>(actual);
        }

        [Fact]
        public void RomanianIsSingleton()
        {
            var expected = Numeral.Romanian;
            var actual = Numeral.Romanian;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void GermanIsCorrect()
        {
            var actual = Numeral.German;
            Assert.IsAssignableFrom<GermanNumeralConverter>(actual);
        }

        [Fact]
        public void GermanIsSingleton()
        {
            var expected = Numeral.German;
            var actual = Numeral.German;
            Assert.Same(expected, actual);
        }

        [Fact]
        public void BrazilianIsCorrect()
        {
            var actual = Numeral.Brazilian;
            Assert.IsAssignableFrom<BrazilianNumeralConverter>(actual);
        }

        [Fact]
        public void BrazilianIsSingleton()
        {
            var expected = Numeral.Brazilian;
            var actual = Numeral.Brazilian;
            Assert.Same(expected, actual);
        }
    }
}
