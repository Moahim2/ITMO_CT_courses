package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;


import java.text.Format;
import java.text.ParsePosition;
import java.util.*;
import java.util.function.Supplier;

public abstract class NumberValueStatistic<T> extends TokenStatistics<T> {

    private final Format formatInput;
    private final Format formatOutput;

    public NumberValueStatistic(String text, Locale textLocale, OutputLocale outLocaleName, Comparator<? super T> comparator, Keys keys) {
        super(text, textLocale, outLocaleName, comparator, keys);
//        Comparator.comparingDouble(Number::doubleValue)
        this.formatInput = getFormatInstance(textLocale);
        this.formatOutput = getFormatInstance(outLocaleName.getLocale());

    }

    abstract protected Format getFormatInstance(final Locale locale);


    @SuppressWarnings("unchecked")
    @Override
    protected List<T> parse(final String text) {
        final ArrayList<T> list = new ArrayList<>();
        final ParsePosition position = new ParsePosition(0);
        while (position.getIndex() != text.length()) {
            int lastPos = position.getIndex();
            T token = (T) formatInput.parseObject(text, position);
            if (lastPos < position.getIndex()) {
                list.add(token);
            } else {
                position.setIndex(lastPos + 1);
            }
        }
        return list;
    }


    @Override
    protected String getExtraToken(final Supplier<T> typeImpl, final String extraKey) {
        T ans = typeImpl.get();
        return getFinalFormattedStr(extraKey, formatOutput.format(estimate(ans)));
    }

    @Override
    protected String getMediumTokenSize() {
        return getFinalFormattedStr(keys.mediumTokenSizeKey, formatOutput.format(getMediumTokenImpl()));
    }

}
