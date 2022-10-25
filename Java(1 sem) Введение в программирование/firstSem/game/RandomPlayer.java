package game;

import java.util.Random;

public class RandomPlayer implements Player {
    private final Random random = new Random();
    private final int countRow;
    private final int countColumn;

    public RandomPlayer(int countRow, int countColumn) {
        this.countRow = countRow;
        this.countColumn = countColumn;
    }
    @Override
    public Move makeMove(Position position) {
        while (true) {
            final Move move = new Move(
                    random.nextInt(countRow),
                    random.nextInt(countColumn),
                    position.getTurn()
            );
            if (position.isValid(move)) {
                return move;
            }
        }
    }
}
