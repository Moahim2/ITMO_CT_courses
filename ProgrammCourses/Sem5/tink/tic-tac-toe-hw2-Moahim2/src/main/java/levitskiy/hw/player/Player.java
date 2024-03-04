package levitskiy.hw.player;

import levitskiy.hw.board.Position;
import levitskiy.hw.concepts.Move;

/**
 * Some object that creates potential moves (but doesn't play moves).
 */
public interface Player {
    /**
     * Creates a move based on the current state of the game position.
     *
     * @param position current situation.
     * @return new move.
     */
    Move makeMove(Position position);
}
