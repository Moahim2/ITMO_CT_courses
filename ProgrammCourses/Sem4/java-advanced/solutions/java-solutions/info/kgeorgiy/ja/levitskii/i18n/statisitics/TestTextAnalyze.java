package info.kgeorgiy.ja.levitskii.i18n.statisitics;


import info.kgeorgiy.ja.levitskii.i18n.OutputLocale;
import info.kgeorgiy.ja.levitskii.i18n.TextAnalyze;
import info.kgeorgiy.ja.levitskii.i18n.TextIOException;
import org.junit.*;
import org.junit.runners.MethodSorters;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.NumberFormat;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static info.kgeorgiy.ja.levitskii.i18n.AnalyzeUtil.CHARSET;
import static info.kgeorgiy.ja.levitskii.i18n.statisitics.TestUtil.*;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestTextAnalyze {
    private final static Path TEST_INPUT_FILE_NAME = Path.of("testInput.txt");
    private static final Path TEST_OUTPUT_FILE_NAME = Path.of("testOutput.txt");

    private static final Path TEST_HW_INPUT_FILE_NAME = Path.of("HWTestInput.txt");
    private static final Path TEST_HW_OUTPUT_FILE_NAME = Path.of("HWTestOutput.txt");
    private static final Path TEST_HW_CHECK_RU_FILE_NAME = Path.of("HWTestCheckRU.txt");
    private static final Path TEST_HW_CHECK_US_FILE_NAME = Path.of("HWTestCheckUS.txt");

    private static final double DELTA = 0.01;




    private final Path testCommonInputFile;
    private final Path testCommonOutputFile;

    private final Path testHWTestInputFile;
    private final Path testHWTestOutputFile;
    private final Path testHWTestCheckRUFile;
    private final Path testHWTestCheckUSFile;
    {
        Path pathToTextsDir = null;
        try {
            pathToTextsDir = getParentPath().resolve("texts");
        } catch (URISyntaxException e) {
            System.err.println("Cannot start tests");
        }
        assert pathToTextsDir != null;


        testCommonInputFile = pathToTextsDir.resolve(TEST_INPUT_FILE_NAME);
        testCommonOutputFile = pathToTextsDir.resolve(TEST_OUTPUT_FILE_NAME);

        testHWTestInputFile = pathToTextsDir.resolve(TEST_HW_INPUT_FILE_NAME);
        testHWTestOutputFile = pathToTextsDir.resolve(TEST_HW_OUTPUT_FILE_NAME);
        testHWTestCheckRUFile = pathToTextsDir.resolve(TEST_HW_CHECK_RU_FILE_NAME);
        testHWTestCheckUSFile = pathToTextsDir.resolve(TEST_HW_CHECK_US_FILE_NAME);

    }


    @Test
    public void random_test_statistics_1_emptyInput_all_lang() throws IOException {
        testFullStatisticsOnAllLang(0, 0, 0, false, false, false, OutputLocale.RU);
    }


    @Test
    public void random_test_statistics_2_oneWord_RU() throws IOException {
        testFullStatistics(TestUtil.Lang.RU,0, 10, 1, false, false, false, OutputLocale.RU);
    }

    @Test
    public void random_test_statistics_3_oneWord_all_lang() throws IOException {
        testFullStatisticsOnAllLang(0, 10, 1, false, false, false, OutputLocale.RU);
    }


    @Test
    public void random_test_statistics_4_oneBigSentence_only_words_RU() throws IOException {
        testFullStatistics(TestUtil.Lang.RU,200, 40, 1, false, false, false, OutputLocale.US);
    }

    @Test
    public void random_test_statistics_5_oneBigSentence_only_words_all_lang() throws IOException {
        testFullStatisticsOnAllLang(200, 40, 1, false, false, false, OutputLocale.US);
    }


    @Test
    public void random_test_statistics_6_oneBigSentence_number_RU() throws IOException {
        testFullStatistics(TestUtil.Lang.RU,200, 40, 1, true, false, false, OutputLocale.US);
    }

    @Test
    public void random_test_statistics_7_oneBigSentence_number_US() throws IOException {
        testFullStatistics(TestUtil.Lang.US,200, 40, 1, true, false, false, OutputLocale.US);
    }

    @Test
    public void random_test_statistics_8_oneBigSentence_number_Arabic() throws IOException {
        testFullStatistics(TestUtil.Lang.ARABIC,200, 40, 1, true, false, false, OutputLocale.US);
    }

    @Test
    public void random_test_statistics_9_oneBigSentence_amount_RU() throws IOException {
        testFullStatistics(TestUtil.Lang.RU,200, 40, 1, false, true, false, OutputLocale.RU);
    }

    @Test
    public void random_test_statistics_10_oneBigSentence_amount_Arabic() throws IOException {
        testFullStatistics(TestUtil.Lang.ARABIC,200, 40, 1, false, true, false, OutputLocale.RU);
    }

    @Test
    public void random_test_statistics_11_oneBigSentence_date_RU() throws IOException {
        testFullStatistics(TestUtil.Lang.RU,200, 40, 1, false, false, true, OutputLocale.RU);
    }

    @Test
    public void random_test_statistics_12_oneBigSentence_date_Arabic() throws IOException {
        testFullStatistics(TestUtil.Lang.ARABIC,200, 40, 1, false, false, true, OutputLocale.US);
    }

    @Test
    public void random_test_statistics_13_oneBigSentence_date_Arabic() throws IOException {
        testFullStatistics(TestUtil.Lang.ARABIC,200, 40, 1, false, false, true, OutputLocale.US);
    }


    @Test
    public void random_test_statistics_14_oneBigSentence_all_values_all_lang() throws IOException {
        testFullStatisticsOnAllLang(200, 40, 1, true, true, true, OutputLocale.RU);
    }



    @Test
    public void random_test_statistics_15_manySentence_numbers_US() throws IOException {
        testFullStatistics(TestUtil.Lang.RU, 30, 15, 100, true, false, false, OutputLocale.RU);
    }


    @Test
    public void random_test_statistics_16_manySentence_numbers_Arabic() throws IOException {
        testFullStatistics(TestUtil.Lang.ARABIC, 30, 15, 100, true, false, false, OutputLocale.US);
    }


    @Test
    public void random_test_statistics_17_manySentence_numbers_all_lang() throws IOException {
        testFullStatisticsOnAllLang(30, 15, 100, true, false, false, OutputLocale.US);
    }


    @Test
    public void random_test_statistics_18_manySentence_all_values_all_lang() throws IOException {
        testFullStatisticsOnAllLang(30, 15, 100, true, true, true, OutputLocale.RU);
    }


    @Test
    public void random_test_statistics_19_bigTest_all_values_all_lang() throws IOException {
        testFullStatisticsOnAllLang(300, 100, 100, true, true, true, OutputLocale.RU);
    }

    @Test
    public void random_test_statistics_20_veryBigTest_small_sentences_all_values_all_lang() throws IOException {
        testFullStatisticsOnAllLang(5, 1, 100000, true, true, true, null);
    }


    @Test
    public void random_test_statistics_21_veryBigTest_all_values_all_lang() throws IOException {
        testFullStatisticsOnAllLang(300, 25, 1000, true, true, true, null);
    }








    @Test
    public void test_HW_file() throws IOException {  //равенство файлов
        analyzeFile(TestUtil.Lang.RU, OutputLocale.US, testHWTestInputFile, testHWTestOutputFile);
        String expectedAnswerUS = read(testHWTestCheckUSFile);
        String actualAnswerUS = read(testHWTestOutputFile);
        Assert.assertTrue(actualAnswerUS.contains(expectedAnswerUS));


        analyzeFile(TestUtil.Lang.RU, OutputLocale.RU, testHWTestInputFile, testHWTestOutputFile);
        String expectedAnswerRU = read(testHWTestCheckRUFile);
        String actualAnswerRU = read(testHWTestOutputFile);
        Assert.assertTrue(actualAnswerRU.contains(expectedAnswerRU));

    }






    @Test
    public void test_analyze_text_main_0_nulls() {
        runTextAnalyzeMain((String[]) null);
        runTextAnalyzeMain(null, null);
        runTextAnalyzeMain(Locale.US.toString(), OutputLocale.RU.name(), null, testCommonOutputFile.toString());
    }


    @Test
    public void test_analyze_text_main_1_badLength() {
        runTextAnalyzeMain();
        runTextAnalyzeMain(Locale.CHINESE.toString());
        runTextAnalyzeMain(Locale.CHINESE.toString(), OutputLocale.US.name());
        runTextAnalyzeMain(Locale.CANADA_FRENCH.toString(), OutputLocale.RU.name(), testCommonInputFile.toString());
    }

    @Test
    public void test_analyze_text_main_2_badOutLocale() {
        runTextAnalyzeMain(Locale.CANADA_FRENCH.toString(), Locale.CHINESE.toString(), testCommonInputFile.toString(), testCommonOutputFile.toString());
        runTextAnalyzeMain(Locale.US.toString(), Locale.FRENCH.toString(), testCommonInputFile.toString(), testCommonOutputFile.toString());
        runTextAnalyzeMain(Locale.ENGLISH.toString(), Locale.ITALIAN.toString(), testCommonInputFile.toString(), testCommonOutputFile.toString());
    }

    @Test
    public void test_analyze_text_main_3_ioeErrors() {
        runTextAnalyzeMain(
                Locale.CANADA_FRENCH.toString(),
                OutputLocale.RU.name(),
                "random_NAME_test_analyze_text_main_3_ioeErrors",
                testCommonOutputFile.toString());
        runTextAnalyzeMain(
                Locale.US.toString(),
                OutputLocale.US.name(),
                "random_NAME_test_analyze_text_main_3_ioeErrors",
                testCommonOutputFile.toString());
    }



    @Test
    public void test_localization_0_RU() throws IOException {
        TextAnalyze textAnalyze = new TextAnalyze();
        testLocalization(Lang.RU, OutputLocale.RU, listLocaleRu, textAnalyze);
    }

    @Test
    public void test_localization_1_US() throws IOException {
        TextAnalyze textAnalyze = new TextAnalyze();
        testLocalization(Lang.US, OutputLocale.US, listLocaleUS, textAnalyze);
    }

    @Test
    public void test_localization_2_all_lang() throws IOException {
        TextAnalyze textAnalyze = new TextAnalyze();
        for (Lang lang : Lang.values()) {
            testLocalization(lang, OutputLocale.RU, listLocaleRu, textAnalyze);
            testLocalization(lang, OutputLocale.US, listLocaleUS, textAnalyze);
        }
    }



    private void testLocalization(TestUtil.Lang lang, OutputLocale outputLocaleName,
                                  List<String> listLocaleMessage, TextAnalyze textAnalyze) throws IOException {
        textAnalyze.setTextLocale(lang.textLocale);
        textAnalyze.setOutputLocaleName(outputLocaleName);
        for (int i = 0; i < 5; i++) {
            String text = genRandomText(
                    lang, 10, (i + 2) * 2 + 1, i + 11,
                    true, true, true, new TestTextStat(lang.textLocale)
            );
            writeTextToCommonInputFile(text);
            try {
                textAnalyze.setText(testCommonInputFile.toString());
                textAnalyze.analyze(testCommonOutputFile.toString());

                final String[] lines = read(testCommonOutputFile).split(System.lineSeparator());
                checkLocale(outputLocaleName, listLocaleMessage, lines);
            } catch (TextIOException e) {
                Assert.fail(e.getMessage());
            }
        }
    }


    private void checkLocale(OutputLocale outputLocaleName, List<String> listLocaleMessage, String[] lines) {
        for (int i = 0; i < listLocaleMessage.size(); i++) {
            System.out.println("<" + lines[i] + ">");
            Assert.assertTrue("expected contains " + listLocaleMessage.get(i), lines[i].matches(listLocaleMessage.get(i)));
        }
        checkLocaleCase(outputLocaleName, lines);
    }

    private void checkLocaleCase(OutputLocale outputLocaleName, String[] lines) {
        final NumberFormat trueLocaleFormat = NumberFormat.getNumberInstance(outputLocaleName.getLocale());
        for (int i = 0; i < 5; i++) {
            int numberCaseLine = i + 2;

            int diff = parseSecondIntNumber(lines[numberCaseLine], trueLocaleFormat);
            String trueCase;
            if (outputLocaleName.name().equals("RU")) {
                trueCase = listGettersTrueCaseRU.get(i).apply(diff);
            } else if (outputLocaleName.name().equals("US")) {
                trueCase = listGettersTrueCaseUS.get(i).apply(diff);
            } else {
                return;
            }

            Assert.assertTrue(trueCase, lines[numberCaseLine].contains(trueCase));
        }
    }



    private void runTextAnalyzeMain(String... args) {
        TextAnalyze.main(args);
    }

    private void testFullStatisticsOnAllLang(int sentenceLength,
                                             int maxWordLength, int countSentences,
                                             boolean numbers, boolean amounts, boolean dates,
                                             OutputLocale outLocaleName) throws IOException {
        for (final TestUtil.Lang lang : TestUtil.Lang.values()) {
            testFullStatistics(lang, sentenceLength, maxWordLength, countSentences, numbers, amounts, dates, outLocaleName);
        }
    }


    private void testFullStatistics(final TestUtil.Lang lang, int sentenceLength,
                                    int maxWordLength, int countSentences,
                                    boolean numbers, boolean amounts, boolean dates, OutputLocale outLocaleName) throws IOException {
        TestTextStat stat = new TestTextStat(lang.textLocale);
        String text =
                genRandomText(
                        lang, sentenceLength, maxWordLength, countSentences,
                        numbers, amounts, dates, stat
                );
        if (outLocaleName != null) {
            writeTextToCommonInputFile(text);
            analyzeFile(lang, outLocaleName, testCommonInputFile, testCommonOutputFile);
            readAndWriteToConsole(testCommonOutputFile);
        }

        checkStatistics(text, lang, OutputLocale.US, stat);
        checkStatistics(text, lang, OutputLocale.RU, stat);
    }





    private void checkStatistics(String text, TestUtil.Lang lang, OutputLocale outLocaleName, TestTextStat stat) {
        Locale textLocale = lang.textLocale;

        SentenceStatistics sentenceStatistics = new SentenceStatistics(text, textLocale, outLocaleName);
        WordStatistics wordStatistics = new WordStatistics(text, textLocale, outLocaleName);
        NumberStatistics numberStatistics = new NumberStatistics(text, textLocale, outLocaleName);
        AmountStatistics amountStatistics = new AmountStatistics(text, textLocale, outLocaleName);
        DateStatistics dateStatistics = new DateStatistics(text, textLocale, outLocaleName);

        try (final ExecutorService executorService = Executors.newFixedThreadPool(5)) {
            executorService.execute(sentenceStatistics::parseText);
            executorService.execute(wordStatistics::parseText);
            executorService.execute(numberStatistics::parseText);
            executorService.execute(amountStatistics::parseText);
            executorService.execute(dateStatistics::parseText);
            executorService.shutdownNow();
        }

        Assert.assertEquals(stat.countSentences, sentenceStatistics.getCountTokensImpl().getKey().intValue());
        if (stat.countSentences != 0) {
            Assert.assertEquals(stat.minSentence, sentenceStatistics.getMinTokenImpl());
            Assert.assertEquals(stat.maxSentence, sentenceStatistics.getMaxTokenImpl());
            sentenceStatistics.parseChars();
            Assert.assertEquals(
                    stat.minOfSizeSentence.getKey(), sentenceStatistics.getMinOfSizeTokenImpl().getKey()
            );
            Assert.assertEquals(
                    stat.maxOfSizeSentence.getKey(), sentenceStatistics.getMaxOfSizeTokenImpl().getKey()
            );
            Assert.assertEquals(stat.mediumSentence, sentenceStatistics.getMediumTokenImpl(), DELTA);
        }


        Assert.assertEquals(stat.countWords, wordStatistics.getCountTokensImpl().getKey().intValue());
        if (stat.countWords != 0) {
            Assert.assertEquals(stat.minWord, wordStatistics.getMinTokenImpl());
            Assert.assertEquals(stat.maxWord, wordStatistics.getMaxTokenImpl());
            wordStatistics.parseChars();
            Assert.assertEquals(stat.minOfSizeWord.getValue() + " : " + wordStatistics.getMinOfSizeTokenImpl().getValue(),
                    stat.minOfSizeWord.getKey(), wordStatistics.getMinOfSizeTokenImpl().getKey()
            );
            Assert.assertEquals(
                    stat.maxOfSizeWord.getValue() + " : " + wordStatistics.getMaxOfSizeTokenImpl().getValue(),
                    stat.maxOfSizeWord.getKey(), wordStatistics.getMaxOfSizeTokenImpl().getKey()
            );
            Assert.assertEquals(stat.mediumWord, wordStatistics.getMediumTokenImpl(), DELTA);
        }



        Assert.assertEquals(stat.countNumbers, numberStatistics.getCountTokensImpl().getKey().intValue());
        if (stat.countNumbers != 0) {
            Assert.assertEquals(stat.minNumber, numberStatistics.getMinTokenImpl().doubleValue(), DELTA);
            Assert.assertEquals(stat.maxNumber, numberStatistics.getMaxTokenImpl().doubleValue(), DELTA);
            Assert.assertEquals(stat.mediumNumber, numberStatistics.getMediumTokenImpl(), DELTA);
        }

        Assert.assertEquals(stat.countAmounts, amountStatistics.getCountTokensImpl().getKey().intValue());
        if (stat.countAmounts != 0) {
            Assert.assertEquals(stat.minAmount, amountStatistics.getMinTokenImpl().doubleValue(), DELTA);
            Assert.assertEquals(stat.maxAmount, amountStatistics.getMaxTokenImpl().doubleValue(), DELTA);
            Assert.assertEquals(stat.mediumAmount, amountStatistics.getMediumTokenImpl(), DELTA);
        }

        Assert.assertEquals(stat.countDates, dateStatistics.getCountTokensImpl().getKey().intValue());
        if (stat.countDates != 0) {
            Assert.assertEquals(stat.minDate.toString(), dateStatistics.getMinTokenImpl().toString());
            Assert.assertEquals(stat.maxDate.toString(), dateStatistics.getMaxTokenImpl().toString());
            Assert.assertEquals(stat.mediumDate, dateStatistics.getMediumTokenImpl(), DELTA);
        }
    }



    private String read(final Path file) throws IOException {
        StringBuilder text = new StringBuilder();
        try (BufferedReader reader = Files.newBufferedReader(file, CHARSET)) {
            String line;
            while ((line = reader.readLine()) != null) {
                text.append(line).append(System.lineSeparator());
            }
        }
        return text.toString();
    }

    private void readAndWriteToConsole(final Path file) throws IOException {
        System.out.println(read(file));
        System.out.println("------------------------------------------------------------------------------------------------------------------");
    }


    private void analyzeFile(final TestUtil.Lang textLang, OutputLocale outLocaleName, Path inputPath, Path outPath) throws TextIOException {
        TextAnalyze textAnalyze = new TextAnalyze(textLang.textLocale, outLocaleName, inputPath.toString());
        textAnalyze.analyze(outPath.toString());
    }


    private void writeTextToCommonInputFile(final String text) throws IOException {
        try (final BufferedWriter writer = Files.newBufferedWriter(testCommonInputFile, CHARSET)) {
            writer.write(text);
        }
    }

    private static Path getParentPath() throws URISyntaxException {
        return Path.of(TestTextAnalyze.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                .resolve(TestTextAnalyze.class.getPackageName().replace(".", File.separator));
    }

}
