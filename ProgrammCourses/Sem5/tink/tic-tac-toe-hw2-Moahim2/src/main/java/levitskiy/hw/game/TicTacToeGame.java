package levitskiy.hw.game;

import levitskiy.hw.board.Board;
import levitskiy.hw.board.Position;
import levitskiy.hw.board.TicTacToeBoard;
import levitskiy.hw.concepts.Cell;
import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;
import levitskiy.hw.player.human.HumanPlayer;
import levitskiy.hw.player.Player;
import levitskiy.hw.player.robot.RobotPlayException;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Scanner;

import static levitskiy.hw.game.GameUtil.getPlayer;
import static levitskiy.hw.game.GameUtil.printStartAttentionMsg;

/**
 * Realisation of full Tic-tac-toe game on 2 players with custom size of board and custom count for win in line.
 */
public class TicTacToeGame {
    private final Board board;
    private final Player[] players = new Player[2];
    private final List<String> namesOfNumbers = List.of("First", "Second");

    public TicTacToeGame(final Board board, final Player player1, final Player player2) {
        this.board = board;
        players[0] = player1;
        players[1] = player2;
    }

    /**
     * Starting the game from the console.
     *
     * @param args array of arguments for cmd. Example of valid arguments is [3, 3, 3, 2, 1] :
     *             on board 3*3 (3 in line for win); minimax robot vs human.
     */
    public static void main(String[] args) {
        if (args == null || Arrays.stream(args).anyMatch(Objects::isNull) || args.length != 5) {
            printStartAttentionMsg();
            return;
        }

        int countRow;
        int countColumn;
        int countForWinning;
        int typeFirstPlayer;
        int typeSecondPlayer;
        try {
            countRow = Integer.parseInt(args[0]);
            countColumn = Integer.parseInt(args[1]);
            countForWinning = Integer.parseInt(args[2]);
            typeFirstPlayer = Integer.parseInt(args[3]);
            typeSecondPlayer = Integer.parseInt(args[4]);

            if (countRow < 1 || countColumn < 1 || countForWinning < 1) {
                System.out.println("m, n, k must be greater than zero.");
                printStartAttentionMsg();
                return;
            }
        } catch (NumberFormatException e) {
            System.out.println("Enter only numbers in args.");
            printStartAttentionMsg();
            return;
        }

        Scanner input = null;
        if (typeFirstPlayer == 1 || typeSecondPlayer == 1) {
            input = new Scanner(System.in);
        }

        try {
            new TicTacToeGame(
                    new TicTacToeBoard(countRow, countColumn, countForWinning),
                    getPlayer(typeFirstPlayer, input, countRow, countColumn, countForWinning),
                    getPlayer(typeSecondPlayer, input, countRow, countColumn, countForWinning)
            ).play();
        } catch (IllegalArgumentException | RobotPlayException e) {
            System.out.println(e.getMessage());
            printStartAttentionMsg();
            return;
        }

        if (input != null) {
            input.close();
        }
    }

    /**
     * Start the game and work while game is not finish.
     *
     * @return result of game (in the sense of a player).
     * @throws RobotPlayException if the robot player was broken and can't make moves.
     */
    public PlayerResult play() throws RobotPlayException {
        while (!board.getPosition().isFinish()) {
            makePlay(0);
            makePlay(1);
        }
        System.out.println();
        System.out.println("The game finish!!!");

        final Position finalPosition = board.getPosition();
        if (finalPosition.getCurrentResult() == GameResult.WIN) {
            if (finalPosition.getTurn() == Cell.Nought) {
                return PlayerResult.FW;
            } else {
                return PlayerResult.SW;
            }
        } else {
            return PlayerResult.DRAW;
        }
    }

    private void makePlay(int numberOfPlayer) throws RobotPlayException {
        GameResult result = GameResult.UNKNOWN;
        if (!board.getPosition().isFinish()) {
            result = makeMove(numberOfPlayer);
        }
        writeResult(result, numberOfPlayer);
    }

    /**
     * Creates and plays a move.
     *
     * @param numberOfPlayer - number of player who must make move.
     * @return result of game after this move.
     * @throws RobotPlayException if the robot player was broken and can't make moves.
     */
    private GameResult makeMove(int numberOfPlayer) throws RobotPlayException {
        final Player player = players[numberOfPlayer];
        final Position position = board.getPosition();

        while (true) {
            final Move move = player.makeMove(position);
            if (!position.isValid(move)) {
                if (player instanceof HumanPlayer) {
                    System.out.println("Wrong input, replay please!");
                    continue;
                } else {
                    throw new RobotPlayException("The robot has broken down and is not playing by the rules!");
                }
            }
            final GameResult result = board.makeMove(move);

            System.out.println();
            System.out.println(namesOfNumbers.get(numberOfPlayer) + " player: ");
            System.out.println(move);
            System.out.println(board);
            System.out.println("Result: " + result);
            return result;
        }
    }

    private void writeResult(GameResult result, int numberOfPlayer) {
        switch (result) {
            case WIN -> System.out.println(namesOfNumbers.get(numberOfPlayer) + " player won!");
            case DRAW -> System.out.println("Draw");
            default -> {}
        }
    }
}
