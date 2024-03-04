package levitskiy.hw.player.robot;

import levitskiy.hw.board.Position;
import levitskiy.hw.concepts.Move;

/**
 * The robot always makes the first possible move in classic order.
 */
public class VeryStupidPlayer extends RobotPlayer {

    public VeryStupidPlayer(int countRow, int countColumn, int countForWinning) {
        super(countRow, countColumn, countForWinning);
    }

    @Override
    public Move makeMove(Position position) {
        for (int i = 0; i < countRow; i++) {
            for (int j = 0; j < countColumn; j++) {
                final Move move = new Move(i, j, position.getTurn());
                if (position.isValid(move)) {
                    return move;
                }
            }
        }
        throw new RobotPlayException("Very stupid player is broken!");
    }
}
