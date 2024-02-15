package jstest;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class JSEngine implements Engine<Object> {
    public static Path JS_ROOT = Path.of(".");
    public static final String OPTIONS = "--module-path=<js>/graal";

    private final ScriptEngine engine;
    private final String evaluate;
    private final String parse;
    private final String toString;

    public JSEngine(final Path script, final String evaluate, final String parse, final String toString) {
        this.evaluate = evaluate;
        this.parse = parse;
        this.toString = toString;

        try {
            System.setProperty("polyglot.engine.WarnInterpreterOnly", "false");

            engine = new ScriptEngineManager().getEngineByName("Graal.js");
            if (engine == null) {
                System.err.println("Graal.js not found");
                System.err.println("Use the following options to run tests:");
                System.err.println(OPTIONS);
                System.err.println("Where <js> - path to the javascript directory of this repository");
                throw new AssertionError("Graal.js not found");
            }

            engine.put("polyglot.js.allowAllAccess", true);
            engine.put("io", new IO(engine));
            engine.put("global", engine.getContext().getBindings(ScriptContext.ENGINE_SCOPE));

            engine.eval("var println = function() { io.println(Array.prototype.map.call(arguments, String).join(' ')); };");
            engine.eval("var print   = function() { io.print  (Array.prototype.map.call(arguments, String).join(' ')); };");
            engine.eval("var include = function(file) { io.include(file); }");
            engine.eval("var expr;");
        } catch (final ScriptException e) {
            throw new EngineException("Invalid initialization", e);
        }

        try {
            include(script.toString(), engine);
        } catch (final ScriptException e) {
            throw new EngineException("Script error", e);
        }
    }

    private static void include(final String script, final ScriptEngine engine) throws ScriptException {
        final Path scriptPath = JS_ROOT.resolve(script);
        try (final Reader reader = Files.newBufferedReader(scriptPath)) {
            engine.eval(reader);
        } catch (final IOException e) {
            throw new EngineException(String.format("Script '%s' not found", scriptPath), e);
        }
    }

    @Override
    public Result<Object> prepare(final String expression) {
        return parse("eval", expression);
    }

    @Override
    public Result<Object> parse(final String expression) {
        return parse(parse, expression);
    }

    private Result<Object> parse(final String parse, final String expression) {
        return eval(expression, String.format("%s(\"%s\")", parse, expression), Object.class);
    }

    private <T> Result<T> eval(final String context, final String code, final Class<T> token) {
        try {
            final Object result = engine.eval(code);
            if (result == null) {
                throw new EngineException("Result is null", null);
            }
            if (token.isAssignableFrom(result.getClass())) {
                return new Result<>(context, token.cast(result));
            }
            throw new EngineException(String.format(
                    "Expected %s, found \"%s\" (%s)%s",
                    token.getSimpleName(),
                    result,
                    result.getClass().getSimpleName(),
                    context
            ), null);
        } catch (final ScriptException e) {
            throw new EngineException("No error expected in " + context + ": " + e.getMessage(), e);
        }
    }

    @Override
    public Result<Number> evaluate(final Result<Object> prepared, final double[] vars) {
        final String code = String.format(
                "expr%s(%s);",
                evaluate,
                Arrays.stream(vars).mapToObj(v -> String.format("%.20f", v)).collect(Collectors.joining(","))
        );
        return evaluate(prepared, code, Number.class);
    }

    public Result<String> toString(final Result<Object> prepared) {
        return evaluate(prepared, "expr." + toString + "()", String.class);
    }

    private <T> Result<T> evaluate(final Result<Object> prepared, final String code, final Class<T> result) {
        engine.getBindings(ScriptContext.ENGINE_SCOPE).put("expr", prepared.value);
        return eval(String.format("%n    in %s%n    where expr = %s%n", code, prepared.context), code, result);
    }

    @SuppressWarnings({"MethodMayBeStatic", "unused"})
    public static class IO {
        private final ScriptEngine engine;
        public IO(final ScriptEngine engine) {
            this.engine = engine;
        }

        public void print(final String message) {
            System.out.print(message);
        }

        public void println(final String message) {
            System.out.println(message);
        }

        public void include(final String file) throws ScriptException {
            JSEngine.include(file, engine);
        }
    }
}
