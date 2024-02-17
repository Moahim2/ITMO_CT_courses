package levitskiy.hw;

public enum Type {
    Int ("0"),
    Boolean("true");

    private final String defaultValue;

    Type(String defaultValue) {
        this.defaultValue = defaultValue;
    }


    public String getDefaultValue() {
        return defaultValue;
    }
}
