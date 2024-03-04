package levitskiy.hw.game;

import levitskiy.hw.player.Player;
import levitskiy.hw.player.human.HumanPlayer;
import levitskiy.hw.player.robot.MiniMaxPlayer;
import levitskiy.hw.player.robot.VeryStupidPlayer;

import java.util.Scanner;

public class GameUtil {

    public static void printStartAttentionMsg() {
        System.out.println("Please input 5 numbers when:");
        System.out.println("First number is count of rows in game (m).");
        System.out.println();
        System.out.println("Second number is count of columns in game (n).");
        System.out.println();
        System.out.println("Third number is count for win in game (k).");
        System.out.println();
        System.out.println("Fourth number is type of first player in list (1, 2, 3).");
        System.out.println("Fifth number is type of second player in list (1, 2, 3).");
        System.out.println("When 1 - human player, 2 - minimax robot player, 3 - very stupid robot player.");
    }

    public static Player getPlayer(int type, Scanner input, int row, int col, int countForWin) {
        return switch (type) {
            case 1 -> new HumanPlayer(input);
            case 2 -> new MiniMaxPlayer(row, col, countForWin);
            case 3 -> new VeryStupidPlayer(row, col, countForWin);
            default -> throw new IllegalArgumentException("Type of player must be only numbers in list (1, 2, 3).");
        };
    }

}
