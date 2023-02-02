package jstest.expression;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Dialect {
    private final String variable;
    private final String constant;
    private final BiFunction<String, List<String>, String> nary;
    private final Map<String, String> aliases;
    private Function<String, String> renamer = Function.identity();

    public Dialect(final String variable, final String constant, final BiFunction<String, List<String>, String> nary) {
        this(variable, constant, nary, new HashMap<>());
    }

    public Dialect(final String variable, final String constant, final String operation, final String separator) {
        this(
                variable,
                constant,
                (op, args) -> operation.replace("{op}", op).replace("{args}", String.join(separator, args))
        );
    }

    private Dialect(final String variable, final String constant, final BiFunction<String, List<String>, String> nary, final Map<String, String> aliases) {
        this.variable = variable;
        this.constant = constant;
        this.nary = nary;
        this.aliases = aliases;
    }

    public Dialect renamed(final String... renames) {
        assert renames.length % 2 == 0;
        for (int i = 0; i < renames.length; i += 2) {
            aliases.put(renames[i], renames[i + 1]);
        }
        return this;
    }

    public Dialect renamer(final Function<String, String> renamer) {
        this.renamer = renamer;
        return this;
    }

    public String variable(final String name) {
        return String.format(variable, name);
    }

    public String constant(final int value) {
        return String.format(constant, value);
    }

    public static String nullary(final String name) {
        return name;
    }

    public String operation(final String name, final List<String> as) {
        return nary.apply(renamer.apply(aliases.getOrDefault(name, name)), as);
    }
}
