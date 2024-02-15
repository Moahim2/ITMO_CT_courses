package info.kgeorgiy.ja.levitskii.i18n;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class AnalyzeUtil {

    public static final Charset CHARSET = StandardCharsets.UTF_8;

    public final static String ENTER = System.lineSeparator();
    public final static String WS = " ";
    public final static String TAB = WS.repeat(4);



    public static String getQuoteStr(final String str) {
        return "\"" + str + "\"";
    }

    public static String getBracketStr(final String str) {
        return "(" + str + ")";
    }

}
