package game;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        try {
            int countRow = Integer.parseInt(args[0]);
            int countColumn = Integer.parseInt(args[1]);
            int countForWinning = Integer.parseInt(args[2]);
            if (countRow < 1 || countColumn < 1 || countForWinning < 1) {
                throw new IllegalArgumentException("m, n, k must be greater than zero");
            }

            new FourPlayerGame(
                    new TicTacToeBoard(countRow, countColumn, countForWinning),
                    new HumanPlayer(new Scanner(System.in)),
                    new RandomPlayer(countRow, countColumn),
                    new RandomPlayer(countRow, countColumn),
                    //new CheatingPlayer()
                    new HumanPlayer(new Scanner(System.in))
            ).play(true);
        } catch (ArrayIndexOutOfBoundsException e) { // ввод меньше трех чисел
            System.out.println("Enter three numbers, please");
        }
    }
}
