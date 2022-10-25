import java.io.IOException;
import java.util.InputMismatchException;
import java.io.UnsupportedEncodingException;
import java.lang.IllegalStateException;
import java.io.Reader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;

public class MyScannerWithNumberLine {
    private Reader in;
    private char[] buffer = new char[0];
    private int read = 0;
    private int bufferBegin = 0; //буферный указатель
    private char lastChar = 1;
    private boolean finish = false;
    public int numberLine = 1; // номер строки      

    public MyScannerWithNumberLine(InputStream sourse) throws UnsupportedEncodingException {
        this.in = new InputStreamReader(sourse, "utf-8");
    }

    public MyScannerWithNumberLine(String line) {
        this.in = new StringReader(line);
    }

    static boolean isHexDecDigit(char ch) {
        String str = "0123456789ABCDEFabcdef";
        if (str.indexOf(Character.toString(ch)) != -1) {
            return true;
        } else {
            return false;
        }
    }

    static boolean isPunctuationDash(char ch) {
        return Character.getType(ch) == Character.DASH_PUNCTUATION;
    }

    static boolean isWordSymbol(char ch) {
        return (isPunctuationDash(ch) || Character.isLetter(ch) || ch == '\'');
    }

    static boolean isLineFeed(char ch) {
        int c = (int) ch; //lf 10, vt 11, ff 12, cr 13, nel 133, ls 8232, ps 8233
        return (c != 10 && c != 11 && c != 12 && c != 13 && c != 133 && c != 8232 && c != 8233);
    }
    private void bufferedRead() throws IOException {
        char[] buffer = new char[512];
        this.read = in.read(buffer);
        this.buffer = buffer;
        this.bufferBegin = 0;
    }

    public int nextInt() throws IOException {
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        if (!goToNextInt()) {
            throw new InputMismatchException("There are no further numbers");
        }
        int sign = 1;
        StringBuilder sb = new StringBuilder();
        if (this.lastChar == '-') {
            sign = -1;
        }
        while (isHexDecDigit(buffer[this.bufferBegin])) {
            sb.append(buffer[this.bufferBegin]);
            if (this.bufferBegin < this.read - 1) {
                this.bufferBegin++;
            } else {
                bufferedRead();
                if (read == -1) {
                    break;
                }
            }
        }
        lastChar = 1;
        return sign * Integer.parseUnsignedInt(sb.toString(), 16);
    }

    public String nextLine() throws IOException {
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        if (!goToNextLine()) {
            throw new InputMismatchException("There are no further lines");
        }
        StringBuilder sb = new StringBuilder();
        while (buffer[this.bufferBegin] != System.lineSeparator().charAt(0) && read != -1) {
            sb.append(buffer[this.bufferBegin]);
            if (this.bufferBegin < this.read - 1) {
                this.bufferBegin++;
            } else {
                bufferedRead();
                if (read == -1) {
                    break;
                }
            }
        }
        this.bufferBegin++;
        if (this.bufferBegin >= this.read) {
            bufferedRead();
        }
        if (System.lineSeparator().length() == 2) {
            this.bufferBegin++;
        }
        return sb.toString();
    }

    public String nextWord() throws IOException {
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        if (!goToNextWordSymbol()) {
            throw new InputMismatchException("There are no more words");
        }
        StringBuilder sb = new StringBuilder();
        while (isWordSymbol(buffer[this.bufferBegin])) {
            sb.append(Character.toLowerCase(buffer[this.bufferBegin]));
            if (this.bufferBegin < this.read - 1) {
                this.bufferBegin++;
            } else {
                bufferedRead();
                if (read == -1) {
                    break;
                }
            }
        }
        return sb.toString();
    }

    public boolean goToNextInt() throws IOException {
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        while (this.read != -1) {
            for (int i = this.bufferBegin; i < this.read; i++) {
                if (isHexDecDigit(buffer[i])) {
                    this.bufferBegin = i;
                    return true;
                }
                this.lastChar = buffer[i];
            }
            bufferedRead();
        }
        return false;
    }

    public boolean goToNextLine() throws IOException {
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        if (this.bufferBegin < this.read) {
            return true;
        } else {
            bufferedRead();
            return (bufferBegin < this.read);
        }
    }

    public boolean goToNextWordSymbol() throws IOException { // видоизмененный поиск следующего слова (контролирует номер строки)
        if (this.finish) {
            throw new IllegalStateException("This MyScanner is closed");
        }
        while (this.read != -1) {
            if (bufferBegin >= read) {
                bufferedRead();
            }
            while (!isWordSymbol(buffer[this.bufferBegin])) {
                if(!isLineFeed(buffer[this.bufferBegin])) { // отслеживание номера строки
                    numberLine++;
                    this.bufferBegin++;
                    if (this.bufferBegin >= this.read) {
                        bufferedRead();
                    }
                    if (!isLineFeed(buffer[this.bufferBegin])) {
                        this.bufferBegin++;
                    }
                    break;
                } //
                bufferBegin++;
                if (bufferBegin >= read) {
                    break;
                }
            }
            if (bufferBegin >= read) {
                bufferedRead();
            }
            if (isWordSymbol(buffer[this.bufferBegin])) {
                break;
            }
        }
        return (this.read > -1);
    }

    public void close() throws IOException {
        this.in.close();
        this.finish = true;
    }
}