package queue;

import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue{
    protected int size;

    protected abstract void enqueueImpl(Object element);

    public void enqueue(Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
    }


    protected abstract Object elementImpl();

    public Object element() {
        assert size > 0;
        return elementImpl();
    }

    public abstract void dequeueImpl();

    public Object dequeue() {
        assert size > 0;
        Object res = element();
        size--;
        dequeueImpl();
        return res;
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    protected abstract void clearImpl();

    public void clear() {
        size = 0;
        clearImpl();
    }

    protected abstract int indexIfImpl(Predicate<Object> predicate, int index);

    public int indexIf(Predicate<Object> predicate) {
        assert predicate != null;
        int index = 0;
        return indexIfImpl(predicate, index);
    }

    protected abstract int lastIndexIfImpl(Predicate<Object> predicate, int index);

    public int lastIndexIf(Predicate<Object> predicate) {
        assert predicate != null;
        int index = size - 1;
        return lastIndexIfImpl(predicate, index);
    }
}
