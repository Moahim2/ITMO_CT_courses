package levitskiy.hw.board;


import levitskiy.hw.concepts.Cell;
import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.NoSuchElementException;

/**
 * A classic Tic-Tac-Toe board with the possibility of rolling back the last move for the possibility of predictions.
 */
public class MiniMaxTestingTicTacToeBoard extends TicTacToeBoard {

    /**
     * The memory of all the moves made in order since the creation of this board.
     */
    private final Deque<Move> lastMoves = new ArrayDeque<>();

    /**
     * Constructs a board for analysis on a real board (current position).
     *
     * @param position current position.
     * @param countRow m
     * @param countColumn n
     * @param countOfWin k
     */
    public MiniMaxTestingTicTacToeBoard(Position position, int countRow, int countColumn, int countOfWin) {
        super(countRow, countColumn, countOfWin);
        for (int i = 0; i < countRow; i++) {
            for (int j = 0; j < countColumn; j++) {
                final Cell cell = position.getCell(i, j);
                table[i][j] = cell;
                if (cell != Cell.Empty) {
                    emptyCount--;
                }
            }
        }
        curTurn = position.getTurn();
    }

    /**
     * Cancels the previous move (and returns the turn queue).
     *
     * @throws IllegalStateException if
     * are you trying to undo a move that was before the creation of this board.
     */
    public void cancelLastMove() throws IllegalStateException {
        try {
            final Move lastMove = lastMoves.removeLast();
            table[lastMove.row()][lastMove.col()] = Cell.Empty;
            emptyCount++;
            curResult = GameResult.UNKNOWN;
            swapTurn();
        } catch (NoSuchElementException e) {
            throw new IllegalStateException("This board was created on this Move and cannot return it!", e);
        }
    }

    @Override
    protected GameResult getResult(int row, int col) {
        lastMoves.addLast(new Move(row, col, curTurn));
        return super.getResult(row, col);
    }
}
