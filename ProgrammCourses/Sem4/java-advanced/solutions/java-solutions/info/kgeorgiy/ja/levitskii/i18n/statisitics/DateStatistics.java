package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.DateFormat;
import java.text.Format;
import java.util.Comparator;
import java.util.Date;
import java.util.Locale;

public class DateStatistics extends NumberValueStatistic<Date> {

    public DateStatistics(String text, Locale textLocale, OutputLocale outLocaleName) {
        super(text, textLocale, outLocaleName, Comparator.comparingLong(Date::getTime), Keys.Date);
    }

    @Override
    protected Format getFormatInstance(Locale locale) {
        return DateFormat.getDateInstance(DateFormat.MEDIUM, locale);
    }

    @Override
    protected double estimate(Date token) {
        if (token == null) {
            return 0;
        }
        return token.getTime();
    }
}
