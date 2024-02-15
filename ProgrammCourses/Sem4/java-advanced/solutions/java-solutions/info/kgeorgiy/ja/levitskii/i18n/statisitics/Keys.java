package info.kgeorgiy.ja.levitskii.i18n.statisitics;

public enum Keys {
    Sentence ("sentence"),
    Word ("word"),
    Number ("number"),
    Money ("money"),
    Date ("date");






    public final String keyName;
    public final String nameFullStatisticsKey;
    public final String countTokensKey;
    public final String minTokenKey;
    public final String maxTokenKey;
    public final String minOfSizeTokenKey;
    public final String maxOfSizeTokenKey;
    public final String mediumTokenSizeKey;

    public final String countPattern;

    Keys(final String keyName) {
        this.keyName = keyName;
        this.nameFullStatisticsKey = "full_stat_" + keyName;
        this.countTokensKey = "count_" + keyName;
        this.minTokenKey = "min_token_" + keyName;
        this.maxTokenKey = "max_token_" + keyName;
        this.minOfSizeTokenKey = "min_of_size_token_" + keyName;
        this.maxOfSizeTokenKey = "max_of_size_token_" + keyName;
        this.mediumTokenSizeKey = "medium_token_size_" + keyName;

        this.countPattern = "count_pattern_" + keyName;
    }

}
