package info.kgeorgiy.ja.levitskii.i18n;

import info.kgeorgiy.ja.levitskii.i18n.statisitics.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


import static info.kgeorgiy.ja.levitskii.i18n.AnalyzeUtil.*;

public final class TextAnalyze {


    private static final OutputLocale DEFAULT_OUTPUT_LOCALE_NAME = OutputLocale.US;
    private static final Locale DEFAULT_TEXT_LOCALE = Locale.getDefault();


    private String textName = null;
    private String text = null;
    private Locale textLocale = DEFAULT_TEXT_LOCALE;
    private OutputLocale outputLocaleName = DEFAULT_OUTPUT_LOCALE_NAME;
    private ResourceBundle bundle = outputLocaleName.getBundle();




    /**
     * Empty constructor
     */
    public TextAnalyze() {}


    public TextAnalyze(final Locale textLocale) {
        setTextLocale(textLocale);
    }

    public TextAnalyze(final Locale textLocale, final OutputLocale outputLocaleName) {
        this(textLocale);
        setOutputLocaleName(outputLocaleName);
    }

    public TextAnalyze(final Locale textLocale, final OutputLocale outputLocaleName,
                       final String input) throws TextIOException {
        this(textLocale, outputLocaleName);
        setText(input);
    }


    public void setTextLocale(final Locale textLocale) {
        if (this.textLocale == null) {
            this.textLocale = DEFAULT_TEXT_LOCALE;
        } else {
            this.textLocale = textLocale;
        }
    }

    public void setOutputLocaleName(final OutputLocale textLocale) {
        this.outputLocaleName = Objects.requireNonNullElse(textLocale, DEFAULT_OUTPUT_LOCALE_NAME);
        bundle = this.outputLocaleName.getBundle();
    }

    public void setText(final String input) throws TextIOException {
        if (input == null) {
            System.out.println(getLocaleStr("text_null"));
            return;
        }
        this.text = readText(input);
    }



    public static void main(String[] args) {
        ResourceBundle argumentsErrorBundle = DEFAULT_OUTPUT_LOCALE_NAME.getBundle();
        if (args == null) {
            System.err.println(argumentsErrorBundle.getString("main_args_null"));
            return;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println(argumentsErrorBundle.getString("main_args_contains_null"));
            return;
        }
        if (args.length != 4) {
            System.err.println(argumentsErrorBundle.getString("main_args_bad_count"));
            return;
        }

        final Locale inputLocale = Locale.of(args[0]);
        final OutputLocale outputLocaleName;

        try {
            outputLocaleName = OutputLocale.valueOf(args[1]);
        } catch (IllegalArgumentException e) {
            System.err.println(argumentsErrorBundle.getString("main_incorrect_out_locale"));
            return;
        }

        try {
            final TextAnalyze textAnalyze = new TextAnalyze(inputLocale, outputLocaleName, args[2]);
            textAnalyze.analyze(args[3]);
        } catch (TextIOException e) {
            System.err.println(e.getMessage());
        }

    }



    private Path getPath(final String pathName, final String message) throws TextIOException {
        final Path path;
        try {
            path = Paths.get(pathName);
        } catch (InvalidPathException e) { //Paths.get -> throws
            throw new TextIOException(message, e);
        }
        return path;
    }

    private void createParentOutputDir(final Path outFilePath) throws TextIOException {
        final Path outFileParentPath = outFilePath.getParent();
        if (outFileParentPath != null && !Files.exists(outFileParentPath)) {
            try {
                Files.createDirectory(outFileParentPath);
            } catch (IOException e) {
                throw new TextIOException("parent_path_creating_error", e);
            }
        }
    }

    private String readText(final String input)  throws TextIOException {
        final Path path = getPath(input, getLocaleStr("path_input_error"));
        textName = path.getFileName().toString();

        final StringBuilder sb = new StringBuilder();

        String bufLine;
        try (final BufferedReader reader = Files.newBufferedReader(path, CHARSET)){
            while ((bufLine = reader.readLine()) != null) {
                sb.append(bufLine);
                sb.append(System.lineSeparator());
            }
        } catch (IOException e) {
            throw new TextIOException(getLocaleStr("reading_error"), e);
        }

        return sb.toString();
    }

    private void writeResult(final String output, List<TokenStatistics<?>> tokenStatistics) throws TextIOException {
        final Path outFilePath = getPath(output, getLocaleStr("path_output_error"));
        createParentOutputDir(outFilePath);

        try (final BufferedWriter writer = Files.newBufferedWriter(outFilePath, CHARSET)) {
            writer.write(bundle.getString("analyzed_file_name"));
            writer.write(getQuoteStr(textName));
            writer.newLine();


            writer.write(getLocaleStr("summary_statistics"));
            writer.newLine();
            for (TokenStatistics<?> statistics : tokenStatistics) {
                writer.write(statistics.getCountTokens());
            }


            for (TokenStatistics<?> statistic : tokenStatistics) {
                writer.write(statistic.getFullStatistics());
            }
        } catch (IOException e) {
            throw new TextIOException(getLocaleStr("writing_error"), e);
        }
    }

    private String getLocaleStr(final String key) {
        return bundle.getString(key);
    }


    public void analyze(final String output) throws TextIOException {
        if (text == null) {
            throw new TextIOException(getLocaleStr("text_null"));
        }

        final List<TokenStatistics<?>> statistics = List.of(
                new SentenceStatistics(text, textLocale, outputLocaleName),
                new WordStatistics(text, textLocale, outputLocaleName),
                new NumberStatistics(text, textLocale, outputLocaleName),
                new AmountStatistics(text, textLocale, outputLocaleName),
                new DateStatistics(text, textLocale, outputLocaleName)
        );

        try (ExecutorService executorService = Executors.newFixedThreadPool(5)) {
            statistics.forEach(s -> executorService.execute(s::calculateFullStatistics));
            executorService.shutdownNow();
        }

        writeResult(output, statistics);
    }


}
