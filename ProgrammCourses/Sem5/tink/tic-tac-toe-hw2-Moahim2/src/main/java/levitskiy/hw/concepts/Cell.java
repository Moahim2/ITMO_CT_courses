package levitskiy.hw.concepts;

/**
 *  Types of cell filling.
 */
public enum Cell {
    Empty("."),
    Cross("X"),
    Nought("O");

    private final String view;

    Cell(String view) {
        this.view = view;
    }

    @Override
    public String toString() {
        return view;
    }
}
