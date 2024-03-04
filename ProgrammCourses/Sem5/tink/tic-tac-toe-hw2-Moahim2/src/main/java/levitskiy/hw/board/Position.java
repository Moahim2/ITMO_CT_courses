package levitskiy.hw.board;

import levitskiy.hw.concepts.Cell;
import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;

/**
 * This interface makes it possible to look
 * at the current position but not change it (for change look interface {@link Board}).
 */
public interface Position {
    Cell getTurn();

    boolean isValid(Move move);

    boolean isFinish();

    GameResult getCurrentResult();

    Cell getCell(int row, int column);
}
