import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
public class WordStatCount {
    static boolean isPunctuationDash(char ch) {
        return Character.getType(ch) == Character.DASH_PUNCTUATION;
    }

    static int[] intArrayExpansLine(int[] arr) {
        int[] copy = Arrays.copyOf(arr, 2 * arr.length);
        return copy;
    }

    static String[] stringArrayExpansLine(String[] arr) {
        String[] copy = Arrays.copyOf(arr, 2 * arr.length);
        return copy;
    }

    public static void main(String[] args) {
        String nameFileInput = args[0];
        String nameFileOutput = args[1];
        try {
            BufferedReader in = new BufferedReader(
                new InputStreamReader(
                    new FileInputStream(nameFileInput),
                    "utf-8"
                )
            );
            String[] glossary = new String[2];
            int[] countWords = new int[2];
            try {
                char ch;
                StringBuilder sb = new StringBuilder();
                while (true) {
                    int read = in.read();
                    if (read == -1) {
                        break;
                    }
                    ch = (char) read;
                    if (Character.isLetter(ch) || ch == '\'' || isPunctuationDash(ch)) {
                        sb.append(Character.toLowerCase(ch));
                    } else if (sb.length() != 0) {
                        int i = 0;
                        while (i < glossary.length) {
                            if (sb.toString().equals(glossary[i])) {
                                countWords[i] += 1;
                                break;
                            } else if (glossary[i] == null) {
                                glossary[i] = sb.toString();
                                countWords[i] += 1;
                                break;
                            }
                            i++;
                        }
                        if (i == glossary.length) {
                            glossary = stringArrayExpansLine(glossary);
                            countWords = intArrayExpansLine(countWords);
                            glossary[i] = sb.toString();
                            countWords[i] += 1;
                        }
                        sb = new StringBuilder();
                    }
                }
                int j = 0;
                while (j < countWords.length) {
                    if (countWords[j] == 0) {
                        break;
                    }
                    j++;
                }
                glossary = Arrays.copyOf(glossary, j);
                countWords = Arrays.copyOf(countWords, j);
            } finally {
                in.close();
            }
            int[] statCount = new int[countWords.length];        //сортировка по числу вхождения
            String[] glossaryCount = new String[glossary.length];
            int maxCount = 0;
            for (int k = 0; k < countWords.length; k++) {
                if (maxCount < countWords[k]) {
                    maxCount = countWords[k];
                }
            }
            for (int k = 0; k < countWords.length; k++) {
                int[] min = {maxCount + 1, 0};
                for (int s = 0; s < countWords.length; s++) {
                    if (countWords[s] < min[0] && countWords[s] > 0) {
                        min[0] = countWords[s];
                        min[1] = s;
                    }
                }
                countWords[min[1]] = 0;
                statCount[k] = min[0];
                glossaryCount[k] = glossary[min[1]];
            }
            try {
                BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                        new FileOutputStream(nameFileOutput),
                        "utf-8"
                    )
                );
                try {
                for (int i = 0; i < statCount.length; i++) {
                    out.write(glossaryCount[i] + " " + statCount[i]);
                    if (i != statCount.length - 1) {
                        out.newLine();
                    }
                }
                } finally {
                    out.close();
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