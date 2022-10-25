package game;

import java.util.List;

public class FourPlayerGame {
    private final Board board;
    private final Player player1;
    private final Player player2;
    private final Player player3;
    private final Player player4;
    private int countPlayer;
    private final int[] finish = new int[4];

    public FourPlayerGame(Board board, Player player1, Player player2, Player player3, Player player4) {
        this.board = board;
        this.player1 = player1;
        this.player2 = player2;
        this.player3 = player3;
        this.player4 = player4;
        countPlayer = 4;
    }
    private int makePlay(Player player, int no, boolean log) { // сокращаем запись play
        int result = -2;
        if (finish[no - 1] != 1) {
            result = makeMove(player, no, log);
            if (result != -1 && result != 0) {
                finish[no - 1] = 1;
                countPlayer--;
                writeResult(result);
            }
        }
        return result;
    }

    public void play(boolean log) {
        int result = 0;
        while (countPlayer > 1) {
            result = makePlay(player1, 1, log);
            if (result == 0 || countPlayer == 1) {
                break;
            }
            result = makePlay(player2, 2, log);
            if (result == 0 || countPlayer == 1) {
                break;
            }
            result = makePlay(player3, 3, log);
            if (result == 0 || countPlayer == 1) {
                break;
            }
            result = makePlay(player4, 4, log);
            if (result == 0 || countPlayer == 1) {
                break;
            }
        }
        if (result == 0) {
            writeResult(result);
        }
    }

    private int makeMove(Player player, int no, boolean log) {
        while (true) {
            Move move = null;
            try {
                move = player.makeMove(board.getPosition()); //ловим ошибку player (в основном у Random)
            } catch (Exception e) {
                if (player instanceof RandomPlayer) {
                    System.out.println("Player: " + no + " loose");
                    return -2;
                } else {
                    System.out.println("Play by the rules, please!");
                }
            }
            if (player instanceof HumanPlayer) { //ловим неккоректный ввод человека
                if (!board.getPosition().isValid(move)) {
                    System.out.println("Wrong input, replay please!");
                    continue;
                }
            }
            //        if (player instanceof RandomPlayer) {
            //            //System.out.println(move);
            //            if (!board.getPosition().isValid(move)) {
            //                System.out.println("RandomPlayer played incorrectly");
            //                return 3 - no;
            //            }
            //        }
            final GameResult result = board.makeMove(move);
            if (log) {
                System.out.println();
                System.out.println("Player: " + no);
                System.out.println(move);
                System.out.println(board);
                System.out.println("Result: " + result);
            }
            switch (result) {
                case WIN:
                    return no;
                case LOOSE:
                    return List.of(finish).indexOf(0);
                case DRAW:
                    return 0;
                case UNKNOWN:
                    return -1;
                default:
                    throw new AssertionError("Unknown makeMove result " + result);
            }
        }
    }
    private void writeResult(int result) {
        switch (result) {
            case 1:
                System.out.println("First player won");
                break;
            case 2:
                System.out.println("Second player won");
                break;
            case 3:
                System.out.println("Third player won");
                break;
            case 4:
                System.out.println("Fourth player won");
                break;
            case 0:
                System.out.println("Draw");
                break;
            default:
                throw new AssertionError("Unknown result " + result);
        }
    }
}
