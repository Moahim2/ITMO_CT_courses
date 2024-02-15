package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.BreakIterator;
import java.util.Locale;

public class WordStatistics extends LexemeStatistics {

    public WordStatistics(String text, Locale textLocale, OutputLocale outLocale) {
        super(text, textLocale, outLocale, Keys.Word);
    }

    @Override
    protected BreakIterator getTokenIterator() {
        return BreakIterator.getWordInstance(textLocale);
    }

}
