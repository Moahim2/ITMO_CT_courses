package levitskiy.hw.board;

import levitskiy.hw.concepts.Cell;
import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;

import java.util.Arrays;

/**
 * Classic Tic-tac-toe board m*n (k in line for win) only for 2 players where player with "X" has first move.
 */
public class TicTacToeBoard implements Board, Position {
    protected final int countRow;
    protected final int countColumn;
    protected final int countOfWin;

    protected final Cell[][] table;
    protected int emptyCount;

    protected Cell curTurn = Cell.Cross;
    protected GameResult curResult = GameResult.UNKNOWN;

    /**
     *
     * @param countRow count of rows in game (m).
     * @param countColumn count of columns in game (n).
     * @param countOfWin the number of identical elements in a row to win (k).
     */
    public TicTacToeBoard(int countRow, int countColumn, int countOfWin) {
        this.countRow = countRow;
        this.countColumn = countColumn;
        this.countOfWin = countOfWin;

        table = new Cell[countRow][countColumn];
        emptyCount = countRow * countColumn;

        for (Cell[] row : table) {
            Arrays.fill(row, Cell.Empty);
        }
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move) {
        if (isFinish()) {
            return curResult;
        }

        if (!isValid(move)) {
            return GameResult.UNKNOWN;
        }

        int row = move.row();
        int col = move.col();

        table[row][col] = curTurn;
        emptyCount--;

        curResult = getResult(row, col);
        swapTurn();
        return curResult;
    }

    @Override
    public boolean isFinish() {
        return curResult != GameResult.UNKNOWN;
    }

    @Override
    public GameResult getCurrentResult() {
        return curResult;
    }

    @Override
    public Cell getTurn() {
        return curTurn;
    }

    @Override
    public boolean isValid(Move move) {
        return curTurn == move.type()
                && isValidCell(move.row(), move.col())
                && table[move.row()][move.col()] == Cell.Empty;
    }

    @Override
    public Cell getCell(int row, int column) {
        return table[row][column];
    }

    @Override
    public String toString() {
        int maxRowNumberLength = getNumLength(countRow);
        int maxColNumberLength = getNumLength(countColumn);
        String lineSeparator = System.lineSeparator();


        StringBuilder sb = new StringBuilder();

        appendWsToSB(sb, maxRowNumberLength);
        for (int i = 0; i < countColumn; i++) {
            appendWsToSB(sb, maxColNumberLength - getNumLength(i) + 1);
            sb.append(i + 1);
        }

        sb.append(lineSeparator);
        for (int r = 0; r < countRow; r++) {
            sb.append(r + 1);
            appendWsToSB(sb, maxRowNumberLength - getNumLength(r + 1));

            for (int i = 0; i < table[r].length; i++) {
                appendWsToSB(sb, maxColNumberLength);
                sb.append(table[r][i]);
            }
            sb.append(lineSeparator);
        }

        return sb.toString();
    }

    protected GameResult getResult(int row, int col) {
        if (checkWinAndGetResult(row, col) == GameResult.WIN) {
            return GameResult.WIN;
        }
        return checkDrawAndGetResult();
    }

    protected void swapTurn() {
        if (curTurn == Cell.Cross) {
            curTurn = Cell.Nought;
        } else {
            curTurn = Cell.Cross;
        }
    }

    private GameResult checkWinAndGetResult(int curRow, int curCol) {
        final int[] shiftsRow = new int[] {1, 0, -1, -1}; //diag, row, diag, column
        final int[] shiftsCol = new int[] {1, 1, 1, 0};

        for (int i = 0; i < 4; i++) {
            if (isWinPosition(curRow, curCol, shiftsRow[i], shiftsCol[i])) {
                return GameResult.WIN;
            }
        }

        return GameResult.UNKNOWN;
    }

    private GameResult checkDrawAndGetResult() {
        return emptyCount == 0 ? GameResult.DRAW : GameResult.UNKNOWN;
    }

    /**
     * Checks whether a win has appeared on the corresponding line.
     *
     * @param curRow the coordinate of the new cell of vertical.
     * @param curCol the coordinate of the new cell of horizontal.
     * @param shiftRow sets the horizontal slope.
     * @param shiftCol sets the vertical slope.
     * @return true if board has win.
     */
    private boolean isWinPosition(int curRow, int curCol, int shiftRow, int shiftCol) {
        int count = 0;
        for (int i = 0; i < 2 * countOfWin; i++) {
            int row  = curRow - countOfWin * shiftRow + i * shiftRow;
            int col = curCol - countOfWin * shiftCol + i * shiftCol;

            if (isValidCell(row, col) && table[row][col] == curTurn) {
                if (++count == countOfWin) {
                    return true;
                }
            } else {
                count = 0;
            }
        }
        return false;
    }

    private boolean isValidCell(int row, int col) {
        return 0 <= row
                && 0 <= col
                && row < countRow
                && col < countColumn;
    }

    private int getNumLength(int i) {
        return Integer.toString(i).length();
    }

    private void appendWsToSB(final StringBuilder sb, int count) {
        sb.append(" ".repeat(count));
    }
}
