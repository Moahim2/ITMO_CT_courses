package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;

import java.text.*;
import java.util.*;
import java.util.function.Function;

public class TestUtil {
    private final static Random random = new Random(4875043285743285204L);

    public final static List<String> listLocaleRu = List.of(
            "Анализируемый файл \"testInput.txt\"",
            "Сводная статистика",
            "Число предложений: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Число слов: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Число чисел: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Число сумм: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Число дат: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Статистика по предложениям",
            ".*Число предложений: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Минимальное предложение: \".*\"\\.",
            ".*Максимальное предложение: \".*\"\\.",
            ".*Средняя длина предложения: ([0-9]| |,)+\\.",
            ".*Минимальная длина предложения: [0-9]+ \\(\".*\"\\)\\.",
            ".*Максимальная длина предложения: [0-9]+ \\(\".*\"\\)\\.",
            "Статистика по словам",
            ".*Число слов: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Минимальное слово: \".*\"\\.",
            ".*Максимальное слово: \".*\"\\.",
            ".*Средняя длина слова: ([0-9]| |,)+\\.",
            ".*Минимальная длина слова: [0-9]+ \\(\".*\"\\)\\.",
            ".*Максимальная длина слова: [0-9]+ \\(\".*\"\\)\\.",
            "Статистика по числам",
            ".*Число чисел: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Минимальное число: ([0-9]| |,)+\\.",
            ".*Максимальное число: ([0-9]| |,)+\\.",
            ".*Среднее число: ([0-9]| |,)+\\.",
            "Статистика по суммам денег",
            ".*Число сумм: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Минимальная сумма: ([0-9]| |,)+₽\\.",
            ".*Максимальная сумма: ([0-9]| |,)+₽\\.",
            ".*Средняя сумма: ([0-9]| |,)+₽\\.",
            "Статистика по датам",
            ".*Число дат: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Минимальная дата: [0-9]+ .+ [0-9]+ г\\.\\.",
            ".*Максимальная дата: [0-9]+ .+ [0-9]+ г\\.\\.",
            ".*Средняя дата: [0-9]+ .+ [0-9]+ г\\.\\."
    );



    public final static List<String> listLocaleUS = List.of(
            "Analyzed file is \"testInput.txt\"",
            "Summary statistics",
            "Count of sentences: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Count of words: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Count of numbers: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Count of amounts: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Count of dates: [0-9]+ \\([0-9]+ .+\\)\\.",
            "Statistics on sentences",
            ".*Count of sentences: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Minimum sentence: \".*\"\\.",
            ".*Maximum sentence: \".*\"\\.",
            ".*Medium length of sentences: ([0-9]|\\.|,)+\\.",
            ".*Min length of sentences: [0-9]+ \\(\".*\"\\)\\.",
            ".*Max length of sentences: [0-9]+ \\(\".*\"\\)\\.",
            "Statistics on words",
            ".*Count of words: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Minimum word: \".*\"\\.",
            ".*Maximum word: \".*\"\\.",
            ".*Medium length of words: ([0-9]|\\.|,)+\\.",
            ".*Min length of words: [0-9]+ \\(\".*\"\\)\\.",
            ".*Max length of words: [0-9]+ \\(\".*\"\\)\\.",
            "Statistics on numbers",
            ".*Count of numbers: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Minimum number: ([0-9]|\\.|,)+\\.",
            ".*Maximum number: ([0-9]|\\.|,)+\\.",
            ".*Average number: ([0-9]|\\.|,)+\\.",
            "Statistics on amount of money",
            ".*Count of amounts: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Minimum amount: \\$([0-9]|\\.|,)+\\.",
            ".*Maximum amount: \\$([0-9]|\\.|,)+\\.",
            ".*Average amount: \\$([0-9]|\\.|,)+\\.",
            "Statistics on dates",
            ".*Count of dates: [0-9]+ \\([0-9]+ .+\\)\\.",
            ".*Minimum date: .+ [0-9]+, [0-9]+\\.",
            ".*Maximum date: .+ [0-9]+, [0-9]+\\.",
            ".*Average date: .+ [0-9]+, [0-9]+\\."
    );

    private static String getStringCaseFormat(int i, OutputLocale locale, String key) {
        ChoiceFormat choiceFormat = new ChoiceFormat(locale.getBundle().getString(key));
        MessageFormat messageFormat = new MessageFormat("{0}", locale.getLocale());
        messageFormat.setFormatByArgumentIndex(0, choiceFormat);
        return messageFormat.format(new Object[] {
                locale.getFNumberRange().apply(i)
        });
    }

    public final static List<Function<Integer, String>> listGettersTrueCaseUS = List.of(
            i -> getStringCaseFormat(i, OutputLocale.US, Keys.Sentence.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.US, Keys.Word.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.US, Keys.Number.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.US, Keys.Money.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.US, Keys.Date.countPattern)
    );

    public final static List<Function<Integer, String>> listGettersTrueCaseRU = List.of(
            i -> getStringCaseFormat(i, OutputLocale.RU, Keys.Sentence.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.RU, Keys.Word.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.RU, Keys.Number.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.RU, Keys.Money.countPattern),
            i -> getStringCaseFormat(i, OutputLocale.RU, Keys.Date.countPattern)
    );




    public static String genRandomChar(Lang lang) {
        return Character.toString(lang.letters.charAt(getRandomOnMod(lang.letters.length())));
    }

    public static String genRandomLWord(Lang lang, int length) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < length; i++) {
            sb.append(genRandomChar(lang));
        }
        return sb.toString();
    }

    public static String genRandomWord(Lang lang, int maxLength) {
        int length = getRandomOnMod(maxLength) + 1;
        return genRandomLWord(lang, length);
    }

    public static String genRandomSentence(Lang lang, int length, int maxWordLength,
                                           boolean numbers, boolean amounts, boolean dates,
                                           TestTextStat stat) {
        StringBuilder sb = new StringBuilder();
        createWord(lang, stat, sb, maxWordLength, Character.toString(Character.toUpperCase(genRandomChar(lang).charAt(0))));
        sb.append(" ");


        for (int i = 0; i < length; i++) {
            createWord(lang, stat, sb, maxWordLength, "");

            if (numbers && (i == length / 3 + 1)) {
                sb.append(" ");
                createNumber(lang, stat, sb);
                sb.append(" ");
                continue;
            }


            if (amounts && (i == 2 * length / 3 + 1)) {
                sb.append(" ");
                createAmount(lang, stat, sb);
                sb.append(" ");
                continue;
            }

            if (dates && i == 5 * length / 7 + 1) {
                sb.append(" ");
                createDate(lang, stat, sb);
                sb.append(" ");
                continue;
            }

            if (i < length - 1) {
                sb.append(" ");
            }
        }


        sb.append(".");

        if (getRandomOnMod(3) == 2) {
            sb.append(System.lineSeparator());
        }

        return sb.toString();
    }


    public static String genRandomText(Lang lang, int sentenceLength, int maxWordLength, int countSentences,
                                       boolean numbers, boolean amounts, boolean dates,
                                       TestTextStat stat) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < countSentences; i++) {
            String sentence = genRandomSentence(lang, sentenceLength, maxWordLength, numbers, amounts, dates, stat);
            sb.append(sentence);
            if (i < countSentences - 1) {
                sb.append(" ");
            }
            stat.addSentence(sentence.trim());
        }
        return sb.toString();
    }


    protected static int getRandomOnMod(int x) {
        return ((random.nextInt() % x) + x) % x;
    }

    protected static double getDouble() {
        return random.nextDouble() * 10;
    }

    protected static long getAbsLong() {
        return (Math.abs(random.nextLong())) % 10000000000000L;
    }

    protected static String getNumber(Lang lang, double val) {
        return lang.numberFormat.format(val);
    }

    protected static String getAmount(Lang lang, double val) {
        return lang.currencyFormat.format(val);
    }

    protected static String getDate(Lang lang, long val) {
        return lang.dateFormat.format(val);
    }

    protected static void createWord(Lang lang, TestTextStat stat, StringBuilder sb, int maxWordLength, String prefix) {
        String word = prefix + genRandomWord(lang, maxWordLength);
        stat.addWord(word);
        sb.append(word);
    }

    protected static void createNumber(Lang lang, TestTextStat stat, StringBuilder sb) {
        double val = getDouble();
        stat.addNumber(val);
        sb.append(getNumber(lang, val));
    }

    protected static void createAmount(Lang lang, TestTextStat stat, StringBuilder sb) {
        double val = getDouble();
        String str = getAmount(lang, val);

        ParsePosition parsePosition = new ParsePosition(0);
        while (parsePosition.getIndex() != str.length()) {
            int lastPos = parsePosition.getIndex();
            Number p = lang.numberFormat.parse(str, parsePosition);
            if (lastPos < parsePosition.getIndex()) {
                stat.addNumber(p.doubleValue());
            } else {
                parsePosition.setIndex(lastPos + 1);
            }
        }
        stat.addAmount(val);
        sb.append(str);
    }

    protected static void createDate(Lang lang, TestTextStat stat, StringBuilder sb) {
        long val = getAbsLong();
        String str = getDate(lang, val);

        ParsePosition parsePosition = new ParsePosition(0);
        StringBuilder nameMonth = new StringBuilder();
        while (parsePosition.getIndex() != str.length()) {
            int lastPos = parsePosition.getIndex();
            Number p = lang.numberFormat.parse(str, parsePosition);
            if (lastPos < parsePosition.getIndex()) {
                if (nameMonth.length() > 0) {
                    stat.addWord(nameMonth.toString().trim());
                    nameMonth = new StringBuilder();
                }
                stat.addNumber(p.doubleValue());
            } else {
                if (Character.isLetter(str.charAt(lastPos))) {
                    nameMonth.append(str.charAt(lastPos));
                }
                parsePosition.setIndex(lastPos + 1);
            }
        }
        if (nameMonth.length() > 0) {
            stat.addWord(nameMonth.toString().trim());
        }
        stat.addDate(lang.dateFormat.parse(str, new ParsePosition(0)));
        sb.append(str);
    }

    public static int parseSecondIntNumber(String str, NumberFormat format) {
        ParsePosition parsePosition = new ParsePosition(0);
        int ans = 0;
        while (parsePosition.getIndex() != str.length()) {
            int lastPos = parsePosition.getIndex();
            Number p = format.parse(str, parsePosition);
            if (lastPos < parsePosition.getIndex()) {
                ans = p.intValue();
            } else {
                parsePosition.setIndex(lastPos + 1);
            }
        }
        return ans;
    }


    public enum Lang {
        RU (Character.UnicodeBlock.CYRILLIC, OutputLocale.RU.getLocale(), "абвгдеёжзийклмнопрстуфхцчшщэюя"),
        US (Character.UnicodeBlock.BASIC_LATIN, OutputLocale.US.getLocale(), "abcdefghijklmnopqrstuzyx"),
        FRENCH (Character.UnicodeBlock.BASIC_LATIN, Locale.FRENCH, "abcdefghijklmnopqrstuzyx"),
        ARABIC (Character.UnicodeBlock.ARABIC, Locale.of("ar"), null),
        GREEK (Character.UnicodeBlock.GREEK, Locale.of("el"), "αβγδεζηθικλμνξοπρστυφχψω");


        public final String letters;
        public final String digits;
        public final String fullAlphabet;
        public final NumberFormat numberFormat;
        public final NumberFormat currencyFormat;
        public final DateFormat dateFormat;
        public final Locale textLocale;

        Lang(Character.UnicodeBlock lang, Locale locale, String letters) {
            this.letters = Objects.requireNonNullElseGet(letters, () -> getAlphabetLetters(lang));
            this.digits = getAlphabetDigits(lang);
            this.fullAlphabet = letters + digits;


            this.numberFormat = NumberFormat.getNumberInstance(locale);
            this.currencyFormat = NumberFormat.getCurrencyInstance(locale);
            this.dateFormat = DateFormat.getDateInstance(DateFormat.DEFAULT, locale);
            this.textLocale = locale;
        }

        public static String getAlphabetLetters(Character.UnicodeBlock language) {
            StringBuilder sb = new StringBuilder();
            for (int i = Character.MIN_VALUE; i <= Character.MAX_VALUE; i++) {
                char c = (char) i;
                if (Character.UnicodeBlock.of(c) == language && Character.isLetter(c)) {
                    sb.append(c);
                }
            }
            return sb.toString();
        }

        public static String getAlphabetDigits(Character.UnicodeBlock language) {
            StringBuilder sb = new StringBuilder();
            for (int i = Character.MIN_VALUE; i <= Character.MAX_VALUE; i++) {
                char c = (char) i;
                if (Character.UnicodeBlock.of(c) == language && Character.isDigit(c)) {
                    sb.append(c);
                }
            }
            return sb.toString();
        }
    }


}
