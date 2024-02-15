package info.kgeorgiy.ja.levitskii.i18n;

import java.io.IOException;
import java.util.Locale;


public class MainText {


    public static void main(String[] args) throws IOException {
        TextAnalyze textAnalyze = new TextAnalyze(Locale.UK);

        textAnalyze.setOutputLocaleName(OutputLocale.RU);
        textAnalyze.setTextLocale(OutputLocale.RU.getLocale());
        textAnalyze.setText("input3.txt");
        textAnalyze.analyze("outputFile.txt");
    }
}

