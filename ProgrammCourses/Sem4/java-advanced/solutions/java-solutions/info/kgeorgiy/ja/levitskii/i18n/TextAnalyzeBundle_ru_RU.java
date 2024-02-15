package info.kgeorgiy.ja.levitskii.i18n;

import info.kgeorgiy.ja.levitskii.i18n.statisitics.Keys;

import java.util.ListResourceBundle;

public class TextAnalyzeBundle_ru_RU extends ListResourceBundle {


    private final Object[][] CONTENTS = {
            {"reading_error", "Ошибка при чтении."},
            {"writing_error", "Ошибка при записи."},
            {"text_null", "Не указан/не найден анализируемый текст."},
            {"path_input_error", "Плохой путь к входному файла."},
            {"path_output_error", "Плохой путь к выходному файлу."},
            {"parent_path_creating_error", "Нет родительской директории у входного файла и ее не удается создать."},

            {"main_args_null", "Аргументы - нулл."},
            {"main_args_contains_null", "Аргументы содержат нуллы."},
            {"main_args_bad_count", "Неверное число аргументов."},
            {"main_incorrect_out_locale", "Неподдерживаемая локаль вывода."},


            {"analyzed_file_name", "Анализируемый файл "},
            {"summary_statistics", "Сводная статистика"},


            {Keys.Sentence.nameFullStatisticsKey, "Статистика по предложениям"},
            {Keys.Sentence.countTokensKey, "Число предложений: "},
            {Keys.Sentence.countPattern, "-1#нет предложений|0#различных|1#различное|2#различных"},
            {Keys.Sentence.minTokenKey, "Минимальное предложение: "},
            {Keys.Sentence.maxTokenKey, "Максимальное предложение: "},
            {Keys.Sentence.minOfSizeTokenKey, "Минимальная длина предложения: "},
            {Keys.Sentence.maxOfSizeTokenKey, "Максимальная длина предложения: "},
            {Keys.Sentence.mediumTokenSizeKey, "Средняя длина предложения: "},

            {Keys.Word.nameFullStatisticsKey, "Статистика по словам"},
            {Keys.Word.countTokensKey, "Число слов: "},
            {Keys.Word.countPattern, "-1#нет слов|0#различных|1#различное|2#различных"},
            {Keys.Word.minTokenKey, "Минимальное слово: "},
            {Keys.Word.maxTokenKey, "Максимальное слово: "},
            {Keys.Word.minOfSizeTokenKey, "Минимальная длина слова: "},
            {Keys.Word.maxOfSizeTokenKey, "Максимальная длина слова: "},
            {Keys.Word.mediumTokenSizeKey, "Средняя длина слова: "},



            {Keys.Number.nameFullStatisticsKey, "Статистика по числам"},
            {Keys.Number.countTokensKey, "Число чисел: "},
            {Keys.Number.countPattern, "-1#нет чисел|0#различных|1#различное|2#различных"},
            {Keys.Number.minTokenKey, "Минимальное число: "},
            {Keys.Number.maxTokenKey, "Максимальное число: "},
            {Keys.Number.mediumTokenSizeKey, "Среднее число: "},



            {Keys.Money.nameFullStatisticsKey, "Статистика по суммам денег"},
            {Keys.Money.countTokensKey, "Число сумм: "},
            {Keys.Money.countPattern, "-1#нет денег|0#различных|1#различная|2#различных"},
            {Keys.Money.minTokenKey, "Минимальная сумма: "},
            {Keys.Money.maxTokenKey, "Максимальная сумма: "},
            {Keys.Money.mediumTokenSizeKey, "Средняя сумма: "},


            {Keys.Date.nameFullStatisticsKey, "Статистика по датам"},
            {Keys.Date.countTokensKey, "Число дат: "},
            {Keys.Date.countPattern, "-1#нет дат|0#различных|1#различная|2#различных"},
            {Keys.Date.minTokenKey, "Минимальная дата: "},
            {Keys.Date.maxTokenKey, "Максимальная дата: "},
            {Keys.Date.mediumTokenSizeKey, "Средняя дата: "},



    };

    @Override
    protected Object[][] getContents() {
        return CONTENTS;
    }
}

