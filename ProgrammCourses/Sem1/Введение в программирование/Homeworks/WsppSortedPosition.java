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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
public class WsppSortedPosition {
    public static void main(String[] args) {
        String nameFileInput = args[0];
        String nameFileOutput = args[1];
        try {
            MyScannerWithNumberLine in = new MyScannerWithNumberLine(new FileInputStream(nameFileInput));
            Map<String, List<Integer>> dictionary  = new TreeMap<>();
            Map<String, List<Integer>> dictionaryString  = new TreeMap<>();
            try {
                int numberString = in.numberLine;
                int numberWord = 1;
                while (in.goToNextWordSymbol()) {
                    if (numberString < in.numberLine) {
                        numberWord = 1;
                        numberString = in.numberLine;
                    }
                    String newWord = in.nextWord();
                    if (dictionary.containsKey(newWord)) {
                        dictionary.get(newWord).add(numberWord);
                        dictionaryString.get(newWord).add(in.numberLine);
                    } else {
                        dictionary.put(newWord, new ArrayList<Integer>(Arrays.asList(numberWord)));
                        dictionaryString.put(newWord, new ArrayList<Integer>(Arrays.asList(in.numberLine)));
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
                    Set<Map.Entry<String, List<Integer>>> entries = dictionary.entrySet();
                    for (Map.Entry<String, List<Integer>> entry : entries) {
                        out.write(entry.getKey() + " ");
                        out.write(entry.getValue().size() + " ");
                        for (int j = 0; j < (entry.getValue()).size(); j++) {
                            out.write(dictionaryString.get(entry.getKey()).get(j).toString() + ":" + (entry.getValue()).get(j).toString());
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