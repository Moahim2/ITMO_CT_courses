package game;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import static game.Cell.X;

public class TicTacToeBoard implements Board, Position {
    private final int countRow;
    private final int countColumn;
    private final int k;
    private final int[] finish;
    private final ArrayList<Cell> valueCell = new ArrayList<>(Arrays.asList(Cell.values()));
    private GameResult win = GameResult.UNKNOWN;
    private static final Map<Cell, String> CELL_TO_STRING = Map.of(
            Cell.E, ".",
            Cell.X, "X",
            Cell.O, "0",
            Cell.M, "-",
            Cell.L, "|"
    );

    private final Cell[][] field;
    private Cell turn;

    public TicTacToeBoard(int countRow, int countColumn, int countForWinning) {
        this.countRow = countRow;
        this.countColumn = countColumn;
        this.k = countForWinning;
        field = new Cell[countRow][countColumn];
        for (Cell[] row : field) {
            Arrays.fill(row, Cell.E);
        }
        valueCell.remove(0);
        turn = X;
        finish = new int[4];
    }

    @Override
    public Cell getTurn() {
        return turn;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public GameResult makeMove(Move move) {
        if (!isValid(move)) {
            return GameResult.LOOSE;
        }

        field[move.getRow()][move.getCol()] = move.getValue();
        if (checkWin()) {
            win = GameResult.WIN;
        }

        if (checkDraw()) {
            return GameResult.DRAW;
        }

        switch (turn) {
            case X:
                turn = Cell.O;
                if (finish[1] != 1) {
                    break;
                }
            case O:
                turn = Cell.M;
                if (finish[2] != 1) {
                    break;
                }
            case M:
                turn = Cell.L;
                if (finish[3] != 1) {
                    break;
                }
            case L:
                for (int i = 0; i < 4; i++) {
                    if (finish[i] == 0) {
                        turn = valueCell.get(i);
                        break;
                    }
                }
                break;
        }
        if (win == GameResult.WIN) {
            System.out.println(1);
            win = GameResult.UNKNOWN;
            return GameResult.WIN;
        }
        return GameResult.UNKNOWN;
    }

    private boolean checkDraw() {
        int count = 0;
        for (int r = 0; r < countRow; r++) {
            for (int c = 0; c < countColumn; c++) {
                if (field[r][c] == Cell.E) {
                    count++;
                }
            }
        }
        return count == 0;
    }

    private boolean checkWin() {

        for (int c = 0; c < countColumn; c++) {
            for (int r = 0; r < countRow; r++) {
                int countC = 0;
                for (int i = c; i < c + k && i < countColumn; i++) {
                    if (field[r][i] == turn) {
                        countC++;
                    }
                }
                int countR = 0;
                for (int i = r; i < r + k && i < countRow; i++) {
                    if (field[i][c] == turn) {
                        countR++;
                    }
                }
                int countDiagR = 0;
                for (int i = r, j = c; i < r + k && i < countRow && j < c + k && j <countColumn; i++, j++) {
                    if (field[i][j] == turn) {
                        countDiagR++;
                    }
                }
                int countDiagL = 0;
                for (int i = r, j = c; i < r + k && i < countRow && j > c - k && j >= 0; i++, j--) {
                    if (field[i][j] == turn) {
                        countDiagL++;
                    }
                }
                if (countC >= k || countR >= k || countDiagR >= k || countDiagL >= k) {
                    finish[valueCell.indexOf(turn)] = 1;
                    return true;
                }
            }
        }
        return false;
    }

//    private boolean checkWin(int r, int c) {
//        int countC = 0;
//        int countR = 0;
//        int countDiagR = 0;
//        int countDiagL = 0;
//
//        for (int i = Math.max(0, c - k); i < c + k + 1 && i < countColumn && countC < k; i++) {
//            if (field[r][i] == turn) {
//                countC++;
//            } else {
//                countC = 0;
//            }
//        }
//        for (int i = Math.max(0, r - k); i < r + k + 1 && i < countRow && countR < k; i++) {
//            if (field[i][c] == turn) {
//                countR++;
//            } else {
//                countR = 0;
//            }
//        }
//        for (int i = Math.max(0, c - k), j = Math.min(r - (c - i), Math.max(r - k, 0)); i < c + k + 1 && i < countColumn &&
//        j < r + k + 1 && j < countRow && countDiagR < k; i++, j++) {
//            if (field[j][i] == turn) {
//                countDiagR++;
//            } else {
//                countDiagR = 0;
//            }
//        }
//        for (int i = Math.max(0, c - k), j = Math.min(r + (c - i), Math.min(r + k, countRow)); i < c + k + 1 && i < countColumn &&
//        j > r - k - 1 && j >= 0 && countDiagL < k; i++, j--) {
//            if (field[j][i] == turn) {
//                countDiagL++;
//            } else {
//                countDiagL = 0;
//            }
//        }
//        if (countC >= k || countR >= k || countDiagR >= k || countDiagL >= k) {
//            finish[valueCell.indexOf(turn)] = 1;
//            System.out.println(finish[0] + " " + finish[1] + " " + finish[2] + " " + finish[3]);
//            return true;
//        } else {
//            return false;
//        }
//    }

    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < countRow
                && 0 <= move.getCol() && move.getCol() < countColumn
                && field[move.getRow()][move.getCol()] == Cell.E
                && turn == move.getValue();
    }

    @Override
    public Cell getCell(int row, int column) {
        return field[row][column];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(" ");
        sb.append(" ".repeat(Integer.toString((countRow)).length() - 1));
        int maxLength = Integer.toString(countColumn).length();
        for (int i = 0; i < countColumn; i++) {
            sb.append(" ".repeat(maxLength - Integer.toString(i).length() + 1));
            sb.append(i + 1);
        }
        sb.append(System.lineSeparator());
        for (int r = 0; r < countRow; r++) {
            sb.append(r + 1).append(" ".repeat(
                    Integer.toString((countRow)).length() - Integer.toString(r + 1).length()));
            for (int i = 0; i < field[r].length; i++) {
                sb.append(" ".repeat(maxLength)).append(CELL_TO_STRING.get(field[r][i]));
            }
            sb.append(System.lineSeparator());
        }
        sb.setLength(sb.length() - System.lineSeparator().length());
        return sb.toString();
    }
}
