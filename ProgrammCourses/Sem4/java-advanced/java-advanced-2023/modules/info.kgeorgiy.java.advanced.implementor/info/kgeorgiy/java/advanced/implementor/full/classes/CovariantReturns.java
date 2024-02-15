package info.kgeorgiy.java.advanced.implementor.full.classes;

import java.util.Set;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final  class CovariantReturns {
    public static final Class<?>[] OK = {
            CovariantReturns.Parent.class,
            CovariantReturns.StringChild.class,
            CovariantReturns.CharSequenceChild.class,
            CovariantReturns.TypedChild.class
    };

    public abstract static class Parent<T> {
        public abstract T getValue();
    }

    public interface StringParent {
        public abstract String getValue();
    }

    public abstract static class StringChild extends Parent<String> implements StringParent {
//        @Override
//        public abstract String getValue();
    }

    public abstract static class CharSequenceChild extends Parent<CharSequence> {
        @Override
        public abstract String getValue();
    }

    public abstract static class TypedChild<T extends Set<T>> extends Parent<Set<T>> {
        @Override
        public abstract T getValue();
    }
}
