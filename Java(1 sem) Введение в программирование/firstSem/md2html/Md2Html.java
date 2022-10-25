package md2html;

import markup.*;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class Md2Html {
    public final static char charLineSep = System.lineSeparator().charAt(0);

    static boolean containsMarkup(String str) {
        return (str.contains("*") || str.contains("_") || str.contains("-") || str.contains("`") || str.contains("'"));
    }

    static List<Markup> convert(String str) throws IOException {
        List<Markup> markup = new ArrayList<>();
        ScannerMarkup scanStr = new ScannerMarkup(str);
        while (scanStr.goToNextLine()) {
            markup.add(new Text(scanStr.getStringToNextMarkup())); //1
            String typeMarkdown = scanStr.getTypeThisMarkdown();//2
            if (scanStr.goToNextLine()) { //проверка на одиночные *_-, тире между словами и экранирование
                char nextCh = scanStr.currentSymbol();
                if (((containsMarkup(typeMarkdown) && typeMarkdown.length() == 1) && (
                        nextCh == ' ' || nextCh == charLineSep)) || ( //одиночные
                        typeMarkdown.charAt(0) == '-' && typeMarkdown.length() == 1 //тире
                        )
                ) {
                    markup.add(new Text(typeMarkdown));
                    continue;
                }
                if (typeMarkdown.startsWith("\\")) { // экранирование
                    typeMarkdown = typeMarkdown.substring(1);
                    markup.add(new Text(typeMarkdown));
                    continue;
                }
                if (typeMarkdown.equals("'")) {
                    markup.add(new Text(typeMarkdown));
                    continue;
                }

            } else if (containsMarkup(typeMarkdown)){
                markup.add(new Text(typeMarkdown));
                continue;
            }
            List<Markup> converters = convert(scanStr.getStringBetweenMarkup(typeMarkdown));
            switch (typeMarkdown) { //3
                case "_":
                case "*":
                    markup.add(new Emphasis(converters));
                    break;
                case "**":
                case "__":
                    markup.add(new Strong(converters));
                    break;
                case "--":
                    markup.add(new Strikeout(converters));
                    break;
                case "`":
                    markup.add(new Code(converters));
                    break;
                case "''":
                    markup.add(new Quote(converters));
                    break;
                }
            }
        scanStr.close();
        return markup;
    }

    public static void main(String[] args) {
        String nameFileInput = args[0];
        String nameFileOutput = args[1];
        List<String> input = new ArrayList<>();
        List<String> out = new ArrayList<>();
        try {
            ScannerMarkup in = new ScannerMarkup(new FileInputStream(nameFileInput));
            try {
                while(in.goToNextLine()) { //считывание
                    StringBuilder inputParagraph = new StringBuilder();
                    while (in.goToNextLine()) {
                        String str = in.nextLine();
                        if (str.equals(System.lineSeparator())) {
                            break;
                        }
                        inputParagraph.append(str);
                    }
                    if (!inputParagraph.toString().isEmpty()) {
                        input.add(inputParagraph.toString());
                    }
                }

                for (String str : input) {
                    ScannerMarkup scanStrForHeader = new ScannerMarkup(str);
                    int lenHeader = 0; // работа с абзацем/заголовком
                    if (scanStrForHeader.currentSymbol() == '#') {
                        lenHeader = scanStrForHeader.nextOctothorpe().length();
                        if (scanStrForHeader.currentSymbol() != ' ') {
                            lenHeader = 0;
                        }
                    }
                    StringBuilder header = new StringBuilder();
                    String strWithoutHeader;
                    if (lenHeader == 0) {
                        header.append("<p>");
                        strWithoutHeader = str;
                    } else {
                        scanStrForHeader.skipWhiteSpace();
                        header.append("<h").append(lenHeader).append(">");
                        strWithoutHeader = scanStrForHeader.returnAllNotScanString();
                    }

                    Paragraph outputParagraph = new Paragraph(convert(strWithoutHeader)); //конвертация
                    StringBuilder outSb = new StringBuilder();
                    outputParagraph.toHtml(outSb);
                    outSb.deleteCharAt(outSb.length() - 1); //убираем последние символы переноса строки
                    if ((outSb.charAt(outSb.length() - 1)) == charLineSep) { //если символ переноса двойной
                        outSb.deleteCharAt(outSb.length() - 1);
                    }
                    outSb.insert(0, header); //отметка заголовка в начале
                    header.insert(1, '/');
                    outSb.append(header); //отметка заголовка в конце
                    out.add(outSb.toString());
                }
            } finally {
                in.close();
            }
            try {
                try (BufferedWriter output = new BufferedWriter(
                        new OutputStreamWriter(
                                new FileOutputStream(nameFileOutput),
                                StandardCharsets.UTF_8
                        )
                )) {
                    for (int i = 0; i < out.size(); i++) {
                        output.write(out.get(i));
                        if (i < out.size() - 1) {
                            output.newLine();
                        }
                    }
                }
            } catch (FileNotFoundException e) {
                System.out.println(nameFileOutput + "not found: " + e.getMessage());
            } catch (IOException e) {
                System.out.println("Cannot write: " + e.getMessage());
            }
        } catch (FileNotFoundException e) {
            System.out.println(nameFileInput + "not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Cannot read: " + e.getMessage());
        }
    }
}
