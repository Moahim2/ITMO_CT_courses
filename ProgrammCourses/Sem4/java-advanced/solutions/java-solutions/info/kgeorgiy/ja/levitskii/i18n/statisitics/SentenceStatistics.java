package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.BreakIterator;
import java.util.Locale;

public class SentenceStatistics extends LexemeStatistics {

    public SentenceStatistics(String text, Locale textLocale, OutputLocale outLocale) {
        super(text, textLocale, outLocale, Keys.Sentence);
    }


    @Override
    protected BreakIterator getTokenIterator() {
        return BreakIterator.getSentenceInstance(textLocale);
    }
}
