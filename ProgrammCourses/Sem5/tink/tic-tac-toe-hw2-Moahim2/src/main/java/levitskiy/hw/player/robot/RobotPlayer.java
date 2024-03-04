package levitskiy.hw.player.robot;

import levitskiy.hw.player.Player;

/**
 * An abstract robot that knows about the characteristics of the board on which it is playing.
 */
public abstract class RobotPlayer implements Player {
    protected final int countRow;
    protected final int countColumn;
    protected final int countForWinning;

    protected RobotPlayer(int countRow, int countColumn, int countForWinning) {
        this.countRow = countRow;
        this.countColumn = countColumn;
        this.countForWinning = countForWinning;
    }

}
