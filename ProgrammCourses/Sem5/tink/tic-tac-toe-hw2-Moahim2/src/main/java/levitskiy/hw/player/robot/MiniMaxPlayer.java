package levitskiy.hw.player.robot;

import levitskiy.hw.board.MiniMaxTestingTicTacToeBoard;
import levitskiy.hw.board.Position;
import levitskiy.hw.concepts.Cell;
import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Not a losing robot (At least if he is first and board is small m,n < 5 and k <= 3).
 */
public class MiniMaxPlayer extends RobotPlayer {
    /**
     * Constant for calculate {@link #maxRecursionDepth}.
     */
    private static final int MAX_COUNT_OF_OPERATIONS = 10_000_000;

    /**
     * Constant for calculate {@link #maxRecursionDepth}. The maximum depth is not less than this value.
     */
    private static final int MIN_MAX_DEPTH = 4;

    private int maxRecursionDepth;
    private MiniMaxTestingTicTacToeBoard testBoard;
    private Cell myTurn;
    private Move goodMove;


    public MiniMaxPlayer(int countRow, int countColumn, int countForWinning) {
        super(countRow, countColumn, countForWinning);
        getMaxRecursionDepth();
    }

    /**
     * {@inheritDoc}
     *
     * @param position current situation.
     * @return a move leading to the best possible situation (not necessarily the most efficient in terms of speed).
     */
    @Override
    public Move makeMove(Position position) {
        testBoard = new MiniMaxTestingTicTacToeBoard(position, countRow, countColumn, countForWinning);
        myTurn = testBoard.getTurn();

        minimaxFindGoodMoveAndReturnEvaluation(1);
        return goodMove;
    }

    /**
     * Some method (in this implementation arranged very primitively)
     * estimating the optimal depth of the minimax recursion at the current position.
     */
    private void getMaxRecursionDepth() {
        int curDepth = 1;

        int currentCountOfOperations = 1;
        int currentCountOfChoices = countRow * countColumn;

        while (currentCountOfChoices != 0) {
            currentCountOfOperations *= currentCountOfChoices--;
            if (currentCountOfOperations > MAX_COUNT_OF_OPERATIONS) {
                break;
            }
            curDepth++;
        }
        maxRecursionDepth = Math.max(MIN_MAX_DEPTH, curDepth);
    }

    /**
     * Iterates through all possible moves in a position to a given depth with minimax algorithm.
     * After calling this function, starting from depth 1,
     * the field {@link #goodMove} will contain the best move
     * subject to depth restrictions.
     *
     * @param curDepth current depth of recursion.
     * @return current board position evaluation:
     *      <ul>
     *          <li>(10) - if the position is victorious with the right game</li>
     *          <li>(-10) - if the position is losing when the opponent plays correctly</li>
     *          <li>0 - if the position is tied with the correct play of both sides
     *          or is the result unclear due to the depth limitation</li>
     *      </ul>
     */
    private int minimaxFindGoodMoveAndReturnEvaluation(int curDepth) {
        final Position position = testBoard.getPosition();
        final Cell curTurn = testBoard.getTurn();

        boolean type = curTurn == myTurn;

        if (position.isFinish()) {
            int res;
            if (position.getCurrentResult() == GameResult.DRAW) {
                res = 0;
            } else {
                if (type) {
                    res = -10;
                } else {
                    res = 10;
                }
            }
            return res;
        }

        if (curDepth == maxRecursionDepth) {
            return 0;
        }

        final List<Integer> listEvaluations = new ArrayList<>();

        for (int i = 0; i < countRow; i++) {
            for (int j = 0; j < countColumn; j++) {
                if (position.getCell(i, j) == Cell.Empty) {
                    testBoard.makeMove(new Move(i, j, curTurn));
                    listEvaluations.add(minimaxFindGoodMoveAndReturnEvaluation(curDepth + 1));
                    testBoard.cancelLastMove();
                }
            }
        }

        if (type) {
            int max = Collections.max(listEvaluations);
            if (curDepth == 1) {
                findGoodMove(listEvaluations, max, position);
            }
            return max;
        } else {
            return Collections.min(listEvaluations);
        }
    }

    /**
     * Method which analyzes the better move
     * according to the proposed evaluations of all moves in current position.
     *
     * @param listEvaluations evaluate all empty cells in the correct order.
     * @param max the best of the ratings in the {@code listEvaluations}.
     * @param position current position.
     */
    private void findGoodMove(List<Integer> listEvaluations, int max, Position position) {
        int curRealNumberOfCell = 0;
        int indexOfMax = listEvaluations.indexOf(max);

        for (int i = 0; i < countRow; i++) {
            for (int j = 0; j < countColumn; j++) {
                if (Cell.Empty == position.getCell(i, j) && indexOfMax == curRealNumberOfCell++) {
                    goodMove = new Move(i, j, myTurn);
                    break;
                }
            }
        }
    }
}
