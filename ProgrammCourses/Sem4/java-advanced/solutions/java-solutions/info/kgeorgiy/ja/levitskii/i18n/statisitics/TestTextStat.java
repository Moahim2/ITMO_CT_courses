package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import java.text.Collator;
import java.util.Comparator;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

public class TestTextStat {

    private final Comparator<? super  String> compStringsNullsFirst;
    private final Comparator<? super  String> compStringsNullsLast;
    private final Comparator<? super Map.Entry<Integer, String>> compPairsNullsFirst;
    private final Comparator<? super Map.Entry<Integer, String>> compPairsNullsLast;


    public TestTextStat(Locale locale) {
        final Collator collator = Collator.getInstance(locale);
        this.compStringsNullsFirst = Comparator.nullsFirst(collator);
        this.compStringsNullsLast = Comparator.nullsLast(collator);

        Comparator<? super Map.Entry<Integer, String>> c = Comparator.comparingInt(Map.Entry::getKey);
        this.compPairsNullsFirst = Comparator.nullsFirst(c);
        this.compPairsNullsLast = Comparator.nullsLast(c);
    }

    public int countSentences = 0;
    public int countWords = 0;
    public int countNumbers = 0;
    public int countAmounts = 0;
    public int countDates = 0;


    public String maxSentence = null;
    public String minSentence = null;
    public double mediumSentence = 0;
    private double sumSentences = 0;
    public Map.Entry<Integer, String> minOfSizeSentence = null;
    public Map.Entry<Integer, String> maxOfSizeSentence = null;


    public String maxWord = null;
    public String minWord = null;
    public double mediumWord = 0;
    private double sumWords = 0;
    public Map.Entry<Integer, String> minOfSizeWord = null;
    public Map.Entry<Integer, String> maxOfSizeWord = null;


    public double maxNumber = Double.MIN_VALUE;
    public double minNumber = Double.MAX_VALUE;
    public double mediumNumber = 0;
    private double sumNumbers = 0;


    public double maxAmount = Double.MIN_VALUE;
    public double minAmount = Double.MAX_VALUE;
    public double mediumAmount = 0;
    private double sumAmounts = 0;


    public Date maxDate = null;
    public Date minDate = null;
    public double mediumDate = 0;
    private long sumDates = 0;


    private <T> T getMax(T v1, T v2, Comparator<? super T> comp) {
        if (comp.compare(v1, v2) < 0) {
            return v2;
        } else {
            return v1;
        }
    }

    private <T> T getMin(T v1, T v2, Comparator<? super T> comp) {
        return getMax(v1, v2, comp.reversed());
    }



    public void addSentence(String sentence) {
        countSentences++;
        maxSentence = getMax(maxSentence, sentence, compStringsNullsFirst);
        minSentence = getMin(minSentence, sentence, compStringsNullsLast);
        maxOfSizeSentence = getMax(maxOfSizeSentence, Map.entry(sentence.length(), sentence), compPairsNullsFirst);
        minOfSizeSentence = getMin(minOfSizeSentence, Map.entry(sentence.length(), sentence), compPairsNullsLast);
        sumSentences += sentence.length();
        mediumSentence = sumSentences / countSentences;
    }

    public void addWord(final String word) {
        countWords++;
        maxWord = getMax(maxWord, word, compStringsNullsFirst);
        minWord = getMin(minWord, word, compStringsNullsLast);
        maxOfSizeWord = getMax(maxOfSizeWord, Map.entry(word.length(), word), compPairsNullsFirst);
        minOfSizeWord = getMin(minOfSizeWord, Map.entry(word.length(), word), compPairsNullsLast);
        sumWords += word.length();
        mediumWord = sumWords / countWords;
    }

    public void addNumber(final double number) {
        countNumbers++;
        maxNumber = Math.max(maxNumber, number);
        minNumber = Math.min(minNumber, number);
        sumNumbers += number;
        mediumNumber = sumNumbers / countNumbers;
    }

    public void addAmount(final double amount) {
        countAmounts++;
        maxAmount = Math.max(maxAmount, amount);
        minAmount = Math.min(minAmount, amount);
        sumAmounts += amount;
        mediumAmount = sumAmounts / countAmounts;
    }


    private final Comparator<Date> compDateMax = Comparator.nullsFirst(Comparator.comparingLong(Date::getTime));
    private final Comparator<Date> compDateMin = Comparator.nullsLast(Comparator.comparingLong(Date::getTime));


    public void addDate(final Date date) {
        countDates++;
        maxDate = getMax(maxDate, date, compDateMax);
        minDate = getMin(minDate, date, compDateMin);
        sumDates += date.getTime();
        mediumDate =  ((double) sumDates) / countDates;
    }

}
