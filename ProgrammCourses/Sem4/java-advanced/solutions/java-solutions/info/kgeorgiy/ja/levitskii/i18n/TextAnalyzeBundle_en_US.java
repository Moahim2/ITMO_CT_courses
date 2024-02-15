package info.kgeorgiy.ja.levitskii.i18n;

import info.kgeorgiy.ja.levitskii.i18n.statisitics.Keys;

import java.util.ListResourceBundle;

public class TextAnalyzeBundle_en_US extends ListResourceBundle {


    private final Object[][] CONTENTS = {
            {"reading_error", "Reading error."},
            {"writing_error", "Writing error."},
            {"text_null", "The analyzed text is not specified/not found."},
            {"path_input_error", "Bad path to the input file."},
            {"path_output_error", "Bad path to the output file."},
            {"parent_path_creating_error", "There is no parent directory for the input file and it cannot be created."},


            {"main_args_null", "Args array is null."},
            {"main_args_contains_null", "Args contains nulls."},
            {"main_args_bad_count", "Bad count of arguments."},
            {"main_incorrect_out_locale", "Not supporting locale of out."},


            {"analyzed_file_name", "Analyzed file is "},
            {"summary_statistics", "Summary statistics"},


            {Keys.Sentence.nameFullStatisticsKey, "Statistics on sentences"},
            {Keys.Sentence.countTokensKey, "Count of sentences: "},
            {Keys.Sentence.countPattern, "0#no sentences|1#different sentence|2#different sentences"},
            {Keys.Sentence.minTokenKey, "Minimum sentence: "},
            {Keys.Sentence.maxTokenKey, "Maximum sentence: "},
            {Keys.Sentence.minOfSizeTokenKey, "Min length of sentences: "},
            {Keys.Sentence.maxOfSizeTokenKey, "Max length of sentences: "},
            {Keys.Sentence.mediumTokenSizeKey, "Medium length of sentences: "},


            {Keys.Word.nameFullStatisticsKey, "Statistics on words"},
            {Keys.Word.countTokensKey, "Count of words: "},
            {Keys.Word.countPattern, "0#no words|1#different word|2#different words"},
            {Keys.Word.minTokenKey, "Minimum word: "},
            {Keys.Word.maxTokenKey, "Maximum word: "},
            {Keys.Word.minOfSizeTokenKey, "Min length of words: "},
            {Keys.Word.maxOfSizeTokenKey, "Max length of words: "},
            {Keys.Word.mediumTokenSizeKey, "Medium length of words: "},



            {Keys.Number.nameFullStatisticsKey, "Statistics on numbers"},
            {Keys.Number.countTokensKey, "Count of numbers: "},
            {Keys.Number.countPattern, "0#no numbers|1#different number|2#different numbers"},
            {Keys.Number.minTokenKey, "Minimum number: "},
            {Keys.Number.maxTokenKey, "Maximum number: "},
            {Keys.Number.mediumTokenSizeKey, "Average number: "},


            {Keys.Money.nameFullStatisticsKey, "Statistics on amount of money"},
            {Keys.Money.countTokensKey, "Count of amounts: "},
            {Keys.Money.countPattern, "0#no amounts|1#different amount|2#different amounts"},
            {Keys.Money.minTokenKey, "Minimum amount: "},
            {Keys.Money.maxTokenKey, "Maximum amount: "},
            {Keys.Money.mediumTokenSizeKey, "Average amount: "},


            {Keys.Date.nameFullStatisticsKey, "Statistics on dates"},
            {Keys.Date.countTokensKey, "Count of dates: "},
            {Keys.Date.countPattern, "0#no dates|1#different date|2#different dates"},
            {Keys.Date.minTokenKey, "Minimum date: "},
            {Keys.Date.maxTokenKey, "Maximum date: "},
            {Keys.Date.mediumTokenSizeKey, "Average date: "},




    };

    @Override
    protected Object[][] getContents() {
        return CONTENTS;
    }

}
