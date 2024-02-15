package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.Format;
import java.text.NumberFormat;
import java.util.Comparator;
import java.util.Locale;

public class AmountStatistics extends NumberValueStatistic<Number> {

    public AmountStatistics(String text, Locale textLocale, OutputLocale outLocaleName) {
        super(text, textLocale, outLocaleName, Comparator.comparingDouble(Number::doubleValue), Keys.Money);
    }

    @Override
    protected Format getFormatInstance(Locale locale) {
        return NumberFormat.getCurrencyInstance(locale);
    }

    @Override
    protected double estimate(Number token) {
        if (token == null) {
            return 0;
        }
        return token.doubleValue();
    }
}
