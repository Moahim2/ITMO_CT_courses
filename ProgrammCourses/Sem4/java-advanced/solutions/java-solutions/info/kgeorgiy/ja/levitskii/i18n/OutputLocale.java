package info.kgeorgiy.ja.levitskii.i18n;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.function.IntFunction;

public enum OutputLocale {
    RU (
            Locale.of("ru", "RU"),
            TextAnalyzeBundle_ru_RU.class.getCanonicalName(),
            i -> {
                int x;
                if (i == 0) {
                    x = -1;
                } else if ((i % 100) == 11) {
                    x = 11;
                } else {
                    x = i % 10;
                }
                return x;
            }
    ),
    US (
            Locale.US,
            TextAnalyzeBundle_en_US.class.getCanonicalName(),
            i -> i == 0 ? 0 : (i == 1 ? 1 : 2)
    );


    private final Locale locale;
    private final String bundleBaseName;
    private final IntFunction<Integer> getNumberRange;


    OutputLocale(final Locale locale, final String bundleBaseName, final IntFunction<Integer> getNumberRange) {
        this.locale = locale;
        this.bundleBaseName = bundleBaseName;
        this.getNumberRange = getNumberRange;
    }

    public Locale getLocale() {
        return locale;
    }

    public ResourceBundle getBundle() {
        return ResourceBundle.getBundle(bundleBaseName, locale);
    }

    public IntFunction<Integer> getFNumberRange() {
        return getNumberRange;
    }

}
