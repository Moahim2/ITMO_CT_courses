package info.kgeorgiy.java.advanced.implementor.full.classes;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract class ArraysTest<T> {
    public ArraysTest(final T[] elements) {
    }

    abstract T[] getElements();
    abstract void setElements(T[] elements);

    abstract List<T>[] getList();
    abstract void setList(List<T>[] elements);
}
