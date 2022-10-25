import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.io.OutputStreamWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;

public class WordStatInput {
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
            MyScanner in = new MyScanner(new FileInputStream(nameFileInput));
            String[] glossary = new String[2];
            int[] countWords = new int[2];
            try {
                while (in.goToNextWordSymbol()) {
                    String newWord = in.nextWord();
                        int i = 0;
                        while (i < glossary.length) {
                            if (newWord.equals(glossary[i])) {
                                countWords[i] += 1;
                                break;
                            } else if (glossary[i] == null) {
                                glossary[i] = newWord;
                                countWords[i] += 1;
                                break;
                            }
                            i++;
                        }
                        if (i == glossary.length) {
                            glossary = stringArrayExpansLine(glossary);
                            countWords = intArrayExpansLine(countWords);
                            glossary[i] = newWord;
                            countWords[i] += 1;
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
            try {
                BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                        new FileOutputStream(nameFileOutput),
                        "utf-8"
                    )
                );
                try {
                    for (int i = 0; i < countWords.length; i++) {
                        out.write(glossary[i] + " " + countWords[i]);
                        if (i != countWords.length - 1) {
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