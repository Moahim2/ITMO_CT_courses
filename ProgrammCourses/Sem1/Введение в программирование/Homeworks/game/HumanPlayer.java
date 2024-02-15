package game;

import java.util.Scanner;

public class HumanPlayer implements Player {
    private Scanner in;

    public HumanPlayer(Scanner in) {
        this.in = in;
    }

    @Override
    public Move makeMove(Position position) {
        System.out.println();
        System.out.println("Current position");
        System.out.println(position);
        System.out.println("Enter you move for " + position.getTurn());
        int a, b;
        while (true) {
            try {
                String str1 = in.next();
                a = Integer.parseInt(str1);
                String str2 = in.next();
                b = Integer.parseInt(str2);
                break;
            } catch (NumberFormatException e) { // обработка неккоректного ввода в процессе игры
                in.nextLine();
                System.out.println("Enter the numbers, please");
            }
        }
        return new Move(a - 1, b - 1, position.getTurn());
    }
}
