import java.io.IOException;
import java.util.Arrays;
public class FastReverse {
    static int[][] arrayExpansColumn(int[][] array2d) {
        int[][] copy = new int[2 * array2d.length][];
        for (int i = 0; i < array2d.length; i++) {
            copy[i] = Arrays.copyOf(array2d[i], array2d[i].length);
        }
        for (int i = array2d.length; i < 2 * array2d.length; i++) {
            copy[i] = new int[1];
        }
        return copy;
    }
    static int[][] arrayExpansLine(int[][] array2d, int j) {
        int[] copy = Arrays.copyOf(array2d[j], 2 * array2d[j].length);
        array2d[j] = copy;
        return array2d;
    }
    public static void main(String[] args) {
        try {
            MyScanner scannerLines = new MyScanner(System.in);
            int[][] arr = new int[100][100];
            int lineNumber = 0;
            while (scannerLines.goToNextLine()) {
                if (lineNumber == arr.length) {
                arr = arrayExpansColumn(arr);
                }
                String a = scannerLines.nextLine();
                MyScanner scannerNumbers = new MyScanner(a);
                int j = 0;
                while (scannerNumbers.goToNextInt()) {
                    if (j == arr[lineNumber].length) {
                        arr = arrayExpansLine(arr, lineNumber);
                    }
                    arr[lineNumber][j] = scannerNumbers.nextInt();
                    j++;
                }
                arr[lineNumber] = Arrays.copyOf(arr[lineNumber], j);
                scannerNumbers.close();
                lineNumber++;
            }
            lineNumber--;
            scannerLines.close();
            while (lineNumber >= 0) {
                for (int k = arr[lineNumber].length - 1; k >= 0; k--) {
                    System.out.print(arr[lineNumber][k] + " ");
                }
                lineNumber--;
                System.out.println();
            }
        } catch (IOException e) {
            System.out.println(-5);
        }
    }
}