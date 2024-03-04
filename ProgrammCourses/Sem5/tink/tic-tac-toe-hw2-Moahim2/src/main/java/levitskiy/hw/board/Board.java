package levitskiy.hw.board;

import levitskiy.hw.concepts.GameResult;
import levitskiy.hw.concepts.Move;

/**
 * Interface for board for game.
 * The board provides access to the current position {@link Position} and
 * only it can perform some given changes (moves).
 */
public interface Board {

    /**
     * The only way to find out all the information about the current state of the game.
     * @return current situation in game.
     */
    Position getPosition();

    /**
     * The only way to change the board.
     * @param move pre-configured move.
     * @return the result of the game after the move is made.
     */
    GameResult makeMove(Move move);
}
