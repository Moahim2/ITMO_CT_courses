package levitskiy.hw.player.human;

import levitskiy.hw.board.Position;
import levitskiy.hw.concepts.Move;
import levitskiy.hw.player.Player;

import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The implementation of a human player reading the input of a real player from the console.
 */
public class HumanPlayer implements Player {
    private final Scanner in;

    public HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        System.out.println();
        System.out.println("Current position");
        System.out.println(position);
        System.out.println("Enter you move for " + position.getTurn());

        int row, col;
        while (true) {
            try {
                row = in.nextInt();
                col = in.nextInt();
                break;
            } catch (InputMismatchException e) {
                in.nextLine();
                System.out.println("Enter the numbers, please");
            }
        }
        return new Move(row - 1, col - 1, position.getTurn());
    }

}
