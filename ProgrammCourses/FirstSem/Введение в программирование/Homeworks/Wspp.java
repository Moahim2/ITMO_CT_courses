import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
public class Wspp {
    public static void main(String[] args) {
        String nameFileInput = args[0];
        String nameFileOutput = args[1];
        try {
            MyScanner in = new MyScanner(new FileInputStream(nameFileInput));
            Map<String, ArrayList<Integer>> dictionary  = new LinkedHashMap<>();
            try {
                Integer numberWord = 1;
                while (in.goToNextWordSymbol()) {
                    String newWord = in.nextWord();
                    if (dictionary.containsKey(newWord)) {
                        dictionary.get(newWord).add(numberWord);
                    } else {
                        dictionary.put(newWord, new ArrayList<Integer>(Arrays.asList(numberWord)));
                    }
                    numberWord++;
                }
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
                    Set<Map.Entry<String, ArrayList<Integer>>> entries = dictionary.entrySet();
                    for (Map.Entry<String, ArrayList<Integer>> entry : entries) {
                        out.write(entry.getKey() + " ");
                        out.write(entry.getValue().size() + " ");
                        for (int j = 0; j < (entry.getValue()).size(); j++) {
                            out.write((entry.getValue()).get(j).toString());
                            if (j < (entry.getValue()).size() - 1) {
                                out.write(" ");
                            }
                        }
                        out.newLine();
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