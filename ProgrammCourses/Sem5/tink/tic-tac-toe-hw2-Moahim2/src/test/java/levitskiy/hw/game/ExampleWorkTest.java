package levitskiy.hw.game;


import levitskiy.hw.board.TicTacToeBoard;
import levitskiy.hw.player.robot.MiniMaxPlayer;
import levitskiy.hw.player.robot.VeryStupidPlayer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.DisplayName.class)
public class ExampleWorkTest {

    @Test
    public void MinimaxWinStupidPlayerIn3on3on3Board() {
        int row = 3;
        int col = 3;
        int k = 3;
        Assertions.assertEquals(
                PlayerResult.FW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new MiniMaxPlayer(row, col, k),
                        new VeryStupidPlayer(row, col, k)
                ).play()
        );

        Assertions.assertEquals(
                PlayerResult.SW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new VeryStupidPlayer(row, col, k),
                        new MiniMaxPlayer(row, col, k)
                ).play()
        );
    }

    @Test
    public void MinimaxDrawMinimaxIn3on3on3Board() {
        int row = 3;
        int col = 3;
        int k = 3;
        Assertions.assertEquals(
                PlayerResult.DRAW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new MiniMaxPlayer(row, col, k),
                        new MiniMaxPlayer(row, col, k)
                ).play()
        );
    }

    @Test
    public void MinimaxWinMinimaxIn5on5on3Board() {
        int row = 5;
        int col = 5;
        int k = 3;
        Assertions.assertEquals(
                PlayerResult.FW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new MiniMaxPlayer(row, col, k),
                        new MiniMaxPlayer(row, col, k)
                ).play()
        );
    }

    @Test
    public void MinimaxWinMinimaxIn4on5on3Board() {
        int row = 4;
        int col = 5;
        int k = 3;
        Assertions.assertEquals(
                PlayerResult.FW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new MiniMaxPlayer(row, col, k),
                        new MiniMaxPlayer(row, col, k)
                ).play()
        );
    }

    @Test
    public void MinimaxDrawMinimaxIn5on5on5Board() {
        int row = 5;
        int col = 5;
        int k = 5;
        Assertions.assertEquals(
                PlayerResult.DRAW,
                new TicTacToeGame(
                        new TicTacToeBoard(row, col, k),
                        new MiniMaxPlayer(row, col, k),
                        new MiniMaxPlayer(row, col, k)
                ).play()
        );
    }
}
