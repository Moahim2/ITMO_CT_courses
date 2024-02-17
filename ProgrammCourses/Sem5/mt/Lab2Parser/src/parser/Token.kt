package parser

/**
 * This enum represents the different types of terminals tokens.
 */
enum class Token(val word: String) {
    /**
     * Represents an keyword "var".
     */
    VAR("var"),

    /**
     * Represents an "Array".
     */
    ARR("Array"),

    /**
     * Represents an "Map".
     */
    MAP("Map"),

    /**
     * Represents a ",".
     */
    COM(","),

    /**
     * Represents a ":".
     */
    COL(":"),

    /**
     * Represents a name of types and variables.
     */
    NAME("NAME"),

    /**
     * Represents a "<".
     */
    LBR("<"),

    /**
     * Represents a ">".
     */
    RBR(">"),

    /**
     * Represents a ";".
     */
    SCOL(";"),

    /**
     * Represents a finish of string ("$").
     */
    END("$");
}