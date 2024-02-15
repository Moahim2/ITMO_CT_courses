package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.ChoiceFormat;
import java.text.MessageFormat;
import java.util.*;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static info.kgeorgiy.ja.levitskii.i18n.AnalyzeUtil.*;

public abstract class TokenStatistics<T> {

    protected final String text;
    protected List<T> tokens;
    protected final Locale textLocale;
    protected final ResourceBundle bundle;

    private final IntFunction<Integer> getNumberRange;
    private final Comparator<? super T> comparator;

    protected final Keys keys;
    protected final MessageFormat finalStatFormat;

    private String calculatingCountStat = null;
    protected String calculatingStatistics = null;



    protected TokenStatistics(final String text, final Locale textLocale,
                              final OutputLocale outLocaleName,
                              final Comparator<? super T> comparator,
                              final Keys keys) {
        this.text = text;
        this.textLocale = textLocale;
        this.bundle = outLocaleName.getBundle();
        this.getNumberRange = outLocaleName.getFNumberRange();
        this.comparator = comparator;
        this.keys = keys;
        this.finalStatFormat = new MessageFormat("{0}." + ENTER, bundle.getLocale());

    }



    abstract protected List<T> parse(final String text);
    abstract protected double estimate(final T token);

    protected void parseText() {
        if (tokens == null) {
            tokens = parse(text);
        }
    }

    public String getCountTokens() {
        parseText();
        if (calculatingCountStat != null) {
            return calculatingCountStat;
        }

        final Map.Entry<Integer, Integer> p = getCountTokensImpl();
        final int fullCount = p.getKey();
        final int distinctCount = p.getValue();

        final ChoiceFormat choiceFormat = new ChoiceFormat(bundle.getString(keys.countPattern));


        final MessageFormat countFormat = new MessageFormat(
                finalStatFormat.format(new Object[] {
                        getLocaleStr(keys.countTokensKey) + "{0} " + getBracketStr("{1} {2}")
                }),
                bundle.getLocale()
        );
        countFormat.setFormatByArgumentIndex(2, choiceFormat);

        calculatingCountStat = countFormat.format(new Object[] {
                fullCount,
                fullCount != 0 ? distinctCount : "",
                getNumberRange.apply(distinctCount)
        });
        return calculatingCountStat;
    }


    protected abstract String getExtraToken(final Supplier<T> typeImpl, final String extraKey);

    protected String getMinToken() {
        return getExtraToken(this::getMinTokenImpl, keys.minTokenKey);
    }

    protected String getMaxToken() {
        return getExtraToken(this::getMaxTokenImpl, keys.maxTokenKey);
    }

    abstract protected String getMediumTokenSize();

    public void calculateFullStatistics() {
        parseText();
        String finalMessage = "";
        finalMessage += String.format(
                "%s" + TAB + "%s",
                getLocaleStr(keys.nameFullStatisticsKey) + ENTER,
                getCountTokens()
        );
        if (!tokens.isEmpty()) {
            finalMessage += String.format(
                    TAB + "%s" + TAB + "%s" + TAB + "%s",
                    getMinToken(),
                    getMaxToken(),
                    getMediumTokenSize()
            );
        }
        calculatingStatistics = finalMessage;
    }

    public String getFullStatistics() {
        if (calculatingStatistics == null) {
            calculateFullStatistics();
        }
        return calculatingStatistics;
    }


    protected Map.Entry<Integer, Integer> getCountTokensImpl() {
        return Map.entry(
                tokens.size(),
                (int) getStreamTokens().distinct().count()
        );
    }

    protected T getMinTokenImpl() {
        return getExtra(getStreamTokens()::min);
    }

    protected T getMaxTokenImpl() {
        return getExtra(getStreamTokens()::max);
    }



    protected double getMediumTokenImpl() {
        double sum = ((getStreamTokens().mapToDouble(this::estimate).sum()));
        return tokens.size() != 0 ? sum / tokens.size() : 0;
    }


    private T getExtra(final Function<Comparator<? super T>, Optional<T>> extra) {
        return extra.apply(comparator).orElse(null);
    }

    private Stream<T> getStreamTokens() {
        return tokens.stream();
    }

    protected String getLocaleStr(final String key) {
        return bundle.getString(key);
    }

    protected String getFinalFormattedStr(final String prefixKey, final String str) {
        return finalStatFormat.format(new Object[] {
                bundle.getString(prefixKey) + str
        });
    }

}
