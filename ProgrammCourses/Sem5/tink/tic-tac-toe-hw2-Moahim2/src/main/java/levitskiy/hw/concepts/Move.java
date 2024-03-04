package levitskiy.hw.concepts;

public record Move(int row, int col, Cell type) {
    @Override
    public String toString() {
        return String.format("Move(%s, %d, %d)", type, row + 1, col + 1);
    }
}
