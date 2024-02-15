package queue;

import java.util.function.Predicate;

// Сразу для сдачи HW4 вынесены методы в абстрактный для этого вида
public class ArrayQueue extends AbstractQueue {
    private Object[] elements = new Object[2];
    private int start = 1;
    private int end =  1;

    @Override
    protected void enqueueImpl(Object element) {
        ensureCapacity(size + 1);
        elements[end--] = element;
        // :NOTE: совсем для красоты - сделать у val дефолтное значение 0, чтобы не передавать
        end = sumOnMod(end, 0);
    }


    @Override
    protected Object elementImpl() {
        return elements[start];
    }

    @Override
    public void dequeueImpl() {
        elements[start] = null;
        start = sumOnMod(start, -1);
    }

    @Override
    protected void clearImpl() {
        elements = new Object[2];
        end = 1;
        start = 1;
    }

    private void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            Object[] arr = new Object[2 * elements.length];
            if (end == elements.length - 1) {
                System.arraycopy(elements, 0, arr, elements.length, elements.length);
                end = elements.length - 1; // :NOTE: зачем, если в условии if проверка на равенство с этим значением?
                elements = arr; // :NOTE: это можно сделать после всего if, т.к. делаем и здесь, и в else
                start = elements.length - 1;
            } else {
                System.arraycopy(elements, 0, arr, 0, start + 1);
                System.arraycopy(elements, end + 1, arr,
                        start + elements.length + 1, size - start - 1);
                end += elements.length;
                elements = arr;
            }
        }
    }

    // :NOTE: не тестируются методы indexOf и lastIndexOf

    //Контракт:
    //Pred: element != null
    //Post R = n - x: x = max(i_1,...i_k): a[i_j] == element || R == -1 for i=1...n a[i] != element;
    public int indexOf(Object element) {
        assert element != null;
        int index = 0;
        for (int i = start; index != size; i = sumOnMod(i, -1)) {
            if (!elements[i].equals(element)) {
                index++;
            } else {
                return index;
            }
        }
        return -1;
    }
    //Контракт:
    //Pred: element != null
    //Post R = n - x: x = min(i_1,...i_k): a[i_j] == element || R == -1 for i= 1...n a[i] != element;
    public int lastIndexOf(Object element) {
        assert element != null;
        int index = size - 1;
        for (int i = sumOnMod(end, 1); index != -1; i = sumOnMod(i, 1)) {
            if (!elements[i].equals(element)) {
                index--;
            } else {
                return index;
            }
        }
        return -1;
    }

@Override
    protected int indexIfImpl(Predicate<Object> predicate, int index) {
        for (int i = start; index != size; i = sumOnMod(i, -1)) {
            if (!predicate.test(elements[i])) {
                index++;
            } else {
                return index;
            }
        }
        return -1;
    }

    @Override
    protected int lastIndexIfImpl(Predicate<Object> predicate, int index) {
        for (int i = sumOnMod(end, 1); index != -1; i = sumOnMod(i, 1)) {
            if (!predicate.test(elements[i])) {
                index--;
            } else {
                return index;
            }
        }
        return -1;
    }

    private int sumOnMod(int x, int val) {
        return (x + val + elements.length) % elements.length ;
    }
}
