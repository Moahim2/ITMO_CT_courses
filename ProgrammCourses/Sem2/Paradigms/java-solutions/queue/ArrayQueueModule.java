package queue;
//Model: a[1]...a[n];       a[1] - last, a[n] - first
//I: for 1...n: a[i] != null
public class ArrayQueueModule {
    private static Object[] elements = new Object[2];
    private static int size;
    private static int start = 1;
    private static int end = 1;
    //Immutable(n): for i=1...n: a'[i]==a[i]


    //Контракт:
    //Pred: element != null
    //Post: n' == n + 1 && a[1] = element && for i=1...n: a'[i + 1] = a[i]
    public static void enqueue(Object element) {
        assert element != null;
        ensureCapacity(size + 1);
        size++;
        elements[end--] = element;
        end = sumOnMod(end, 0);
    }

    //Контракт:
    //Pred: n >= 1
    //Post: R = a[n] && n' == n && immutable(n)
    public static Object element() {
        assert size > 0;
        return elements[start];
    }

    //Контракт:
    //Pred: n >= 1
    //Post: R == a[n] && n' == n - 1 && immutable(n')
    public static Object dequeue() {
        assert size > 0;
        Object res = elements[start];
        elements[start] = null;
        size--;
        start = sumOnMod(start, -1);
        return res;
    }

    //Контракт:
    //Pred: true
    //Post: R == n && n' == n && immutable(n)
    public static int size() {
        return size;
    }

    //Контракт:
    //Pred: true
    //Post: R == (n > 0) && n' == n && immutable(n)
    public static boolean isEmpty() {
        return size == 0;
    }

    //Контракт:
    //Pred: true
    //Post: n == 0
    public static void clear() {
        elements = new Object[2];
        end = 1;
        start = 1;
        size = 0;
    }


    private static void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            Object[] arr = new Object[2 * elements.length];
            if (end == elements.length - 1) {
                System.arraycopy(elements, 0, arr, elements.length, elements.length);
                end = elements.length - 1;
                elements = arr;
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
    //Контракт:
    //Pred: element != null
    //Post R = n - x: x = max(i_1,...i_k): a[i_j] == element || R == -1 for i=1...n a[i] != element;
    public static int indexOf(Object element) {
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
    public static int lastIndexOf(Object element) {
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

    private static int sumOnMod(int x, int val) {
        return (x + val + elements.length) % elements.length ;
    }
}