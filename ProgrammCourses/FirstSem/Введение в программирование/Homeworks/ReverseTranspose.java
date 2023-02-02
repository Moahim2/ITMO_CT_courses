import java.util.Scanner;
import java.util.Arrays;
public class ReverseTranspose {
    static int[][] arrayExpansColumn(int[][] array2d) {
        int[][] copy = new int[2 * array2d.length][];
        for (int i = 0; i < array2d.length; i++) {
            copy[i] = array2d[i];
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
        Scanner scannerLines = new Scanner(System.in);
        int[][] arr = new int[100][100];
        int lineNumber = 0;
        int maxlength = 0;
        while (scannerLines.hasNextLine()) {
            if (lineNumber == arr.length) {
               arr = arrayExpansColumn(arr);
            }
            String a = scannerLines.nextLine();
            Scanner scannerNumbers = new Scanner(a);
            int number = 0;
            while (scannerNumbers.hasNextInt()) {
                if (number == arr[lineNumber].length) {
                    arr = arrayExpansLine(arr, lineNumber);
                }
                arr[lineNumber][number] = scannerNumbers.nextInt();
                number++;
            }
            arr[lineNumber] = Arrays.copyOf(arr[lineNumber], number);
            if (maxlength <= number) {
                maxlength = number;
            }
            scannerNumbers.close();
            lineNumber++;
        }
        lineNumber--;
        scannerLines.close();
        for (int i = 0; i <= maxlength; i++) {
            for (int j = 0; j <= lineNumber; j++) {
                if (arr[j].length >= i + 1) {
                    System.out.print(arr[j][i] + " ");
                }
            }
            if (i != maxlength) {
                System.out.println();
            }
        }
    }
}