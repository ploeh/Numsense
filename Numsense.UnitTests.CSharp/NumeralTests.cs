﻿using Ploeh.Numsense.ObjectOriented;
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
    }
}
