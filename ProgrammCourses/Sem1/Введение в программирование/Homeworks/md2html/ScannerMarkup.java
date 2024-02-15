package md2html;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.InputMismatchException;
import java.io.UnsupportedEncodingException;
import java.lang.IllegalStateException;
import java.io.Reader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;

public class ScannerMarkup {
    private final Reader in;
    private char[] buffer = new char[0];
    private int read = 0;
    private int bufferBegin = 0;
    public String lastSymbolAfterGet = ""; //для проверки корректности после getStringBetweenMarkup
    private final int lineSepLength = System.lineSeparator().length();
    private final String lineSep = System.lineSeparator();

    public ScannerMarkup(InputStream sourse) throws UnsupportedEncodingException {
        this.in = new InputStreamReader(sourse, StandardCharsets.UTF_8);
    }

    public ScannerMarkup(String line) {
        this.in = new StringReader(line);
    }

    private void bufferedRead() throws IOException {
        char[] buffer = new char[256];
        this.read = in.read(buffer);
        this.buffer = buffer;
        this.bufferBegin = 0;
    }

    private void shiftBufferBegin() throws IOException {
        if (this.bufferBegin < this.read - 1) {
            this.bufferBegin++;
        } else {
            bufferedRead();
        }
    }

    public boolean goToNextLine() throws IOException { // проверяет остались ли еще символы
        if (this.bufferBegin < this.read) {
            return true;
        } else {
            bufferedRead();
            return (bufferBegin < this.read);
        }
    }

    public String nextLine() throws IOException { // считывает строку, включая символы переноса строк
        StringBuilder sb = new StringBuilder();
        while (buffer[this.bufferBegin] != System.lineSeparator().charAt(0) && read != -1) {
            sb.append(buffer[this.bufferBegin]);
            shiftBufferBegin();
        }
        sb.append(lineSep);
        for (int i = 0; i < lineSepLength; i++) {
            shiftBufferBegin();
        }
        return sb.toString();
    }

    public String nextOctothorpe() throws IOException { //считываем следующую ####- последовательность
        StringBuilder sb = new StringBuilder();
        while (buffer[this.bufferBegin] == '#' && read != -1) {
            sb.append(buffer[this.bufferBegin]);
            shiftBufferBegin();
        }
        return sb.toString();
    }

    public String getStringToNextMarkup() throws  IOException { //1) возвращаем всю строку до первого значимого сивола
        StringBuilder sb = new StringBuilder();
        String markupSymbols = "*_-`\\'";
        while (markupSymbols.indexOf(buffer[this.bufferBegin]) == -1 && read != -1) {
            sb.append(buffer[this.bufferBegin]);
            shiftBufferBegin();
        }
        return sb.toString();
    }

    public String getTypeThisMarkdown() throws  IOException { //2) находясь на маркап-символе считаем его полностью
        StringBuilder sb = new StringBuilder();
        sb.append(this.buffer[this.bufferBegin]);
        shiftBufferBegin(); // далее доп проверка на экранирование и двойной маркап
        if ((buffer[bufferBegin] == sb.toString().charAt(0) || sb.toString().charAt(0) == '\\') && read != -1) {
            sb.append(this.buffer[this.bufferBegin]);
            shiftBufferBegin();
        }
        return sb.toString();
    }

    public String getStringBetweenMarkup(String typeMarkup) throws IOException { //3) возвращает всю строку между данным маркапом
        StringBuilder sb = new StringBuilder();
        while (true) {
            while (this.buffer[this.bufferBegin] != typeMarkup.charAt(0) && read != -1) {
                sb.append(this.buffer[this.bufferBegin]);
                lastSymbolAfterGet = Character.toString(this.buffer[this.bufferBegin]);
                shiftBufferBegin();
            }
            lastSymbolAfterGet = Character.toString(this.buffer[this.bufferBegin]);
            if (typeMarkup.length() == 2 && read != -1) {
                shiftBufferBegin();
                if (typeMarkup.charAt(0) != this.buffer[this.bufferBegin]) {
                    sb.append(lastSymbolAfterGet);
                    continue;
                }
                shiftBufferBegin();
                return sb.toString();
            }
            shiftBufferBegin();
            if (this.buffer[this.bufferBegin] == typeMarkup.charAt(0) && read != -1) {
                sb.append(lastSymbolAfterGet).append(lastSymbolAfterGet);
                shiftBufferBegin();
                continue;
            }
            return sb.toString();
        }
    }

    public char currentSymbol() throws IOException { // читает (без сдвига) текущий символ
        if (read == 0) {
            bufferedRead();
        }
        if (read == -1) {
            throw new InputMismatchException("There are no more token");
        }
        return this.buffer[this.bufferBegin];
    }

    public String returnAllNotScanString() throws IOException { //возвращение всей непрочитанной части строки
        StringBuilder sb = new StringBuilder();
        while (read != -1) {
            sb.append(this.buffer[this.bufferBegin]);
            shiftBufferBegin();
        }
        return sb.toString();
    }

    public void skipWhiteSpace() throws IOException { // пропуск всех первых пробелов
        while (buffer[this.bufferBegin] == ' ' && read != -1) {
            shiftBufferBegin();
        }
    }

    public void close() throws IOException {
        this.in.close();
    }
}
