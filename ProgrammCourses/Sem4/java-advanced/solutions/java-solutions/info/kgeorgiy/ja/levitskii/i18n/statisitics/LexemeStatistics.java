package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.*;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static info.kgeorgiy.ja.levitskii.i18n.AnalyzeUtil.*;


public abstract class LexemeStatistics extends TokenStatistics<String> {
    private final NumberFormat numberFormat;

    private final BreakIterator tokenIterator = getTokenIterator();

    private final List<Integer> tokensWordSizes = new ArrayList<>();

    protected LexemeStatistics(final String text, final Locale textLocale, final OutputLocale outLocale, final Keys keys) {
        super(text, textLocale, outLocale, Collator.getInstance(textLocale), keys);

        this.numberFormat = NumberFormat.getNumberInstance(outLocale.getLocale());
    }


    abstract protected BreakIterator getTokenIterator();

    private List<String> parseOnIterator(final String string, final BreakIterator breakIterator) {
        final ArrayList<String> list = new ArrayList<>();
        breakIterator.setText(string);

        //by kgeorgiy's code
        for (
                int begin = breakIterator.first(), end = breakIterator.next();
                end != BreakIterator.DONE;
                begin = end, end = breakIterator.next()
        ) {

            String token = string.substring(begin, end);
            String trimToken = token.trim();
            if (trimToken.chars().anyMatch(Character::isLetter)) {
                list.add(trimToken);
            }
        }
        return list;
    }

    @Override
    protected List<String> parse(final String string) {
        return parseOnIterator(string, tokenIterator);
    }

    protected void parseChars() {
        if (tokensWordSizes.isEmpty()) {
            for (String token : tokens) {
                tokensWordSizes.add(token.length());
            }
        }
    }

    @Override
    protected double estimate(final String token) {
        return token.length();
    }


    private String getExtraOfSizeToken(final Supplier<Map.Entry<Integer, String>> typeImpl, final String extraKey) {
        Map.Entry<Integer, String> ans = typeImpl.get();
        return getFinalFormattedStr(
                extraKey, numberFormat.format(ans.getKey()) + WS + getBracketStr(getQuoteStr(ans.getValue()))
        );
    }

    private String getMinOfSizeToken() {
        return getExtraOfSizeToken(this::getMinOfSizeTokenImpl, keys.minOfSizeTokenKey);
    }

    private String getMaxOfSizeToken() {
        return getExtraOfSizeToken(this::getMaxOfSizeTokenImpl, keys.maxOfSizeTokenKey);
    }

    protected Map.Entry<Integer, String> getMinOfSizeTokenImpl() {
        return getExtraOfSizeToken(getStreamExtraSizesTokens()::min);
    }

    protected Map.Entry<Integer, String> getMaxOfSizeTokenImpl() {
        return getExtraOfSizeToken(getStreamExtraSizesTokens()::max);
    }


    private Stream<Integer> getStreamExtraSizesTokens() {
        return IntStream.range(0, tokens.size()).boxed();
    }

    private Map.Entry<Integer, String> getExtraOfSizeToken(Function<Comparator<? super Integer>, Optional<Integer>> extra) {
        String ans = extra.apply(Comparator.comparingInt(tokensWordSizes::get))
                .map(tokens::get)
                .orElse("");
        return Map.entry(ans.length(), ans);
    }


    @Override
    protected String getExtraToken(final Supplier<String> typeImpl, final String extraKey) {
        String ans = typeImpl.get();
        return getFinalFormattedStr(extraKey, getQuoteStr(ans));
    }


    @Override
    protected String getMediumTokenSize() {
        return getFinalFormattedStr(keys.mediumTokenSizeKey, numberFormat.format(getMediumTokenImpl()));
    }

    @Override
    public void calculateFullStatistics() {
        super.calculateFullStatistics();
        if (!tokens.isEmpty()) {
            parseChars();
            calculatingStatistics = String.format(
                    "%s" + TAB + "%s" + TAB + "%s",
                    super.calculatingStatistics,
                    getMinOfSizeToken(),
                    getMaxOfSizeToken()
            );
        }
    }
}
