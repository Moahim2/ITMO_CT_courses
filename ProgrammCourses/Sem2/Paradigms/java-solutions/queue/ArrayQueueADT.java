package queue;

//Model: a[1]...a[n];       a[1] - last, a[n] - first
//I: for 1...n: a[i] != null
public class ArrayQueueADT {
    private Object[] elements = new Object[2];
    private int size;
    private int start = 1;
    private int end =  1;
    //Immutable(queue, n): for i=1...n: a'[i]==a[i]


    //Контракт:
    //Pred: element != null && queue != null
    //Post: queue: n' == n + 1 && a[1] = element && for i=1...n: a'[i + 1] = a[i]
    public static void enqueue(ArrayQueueADT queue,Object element) {
        assert element != null;
        ensureCapacity(queue,queue.size + 1);
        queue.size++;
        queue.elements[queue.end--] = element;
        queue.end = sumOnMod(queue, queue.end, 0);
    }

    //Контракт:
    //Pred: n >= 1 && queue !=  null
    //Post: queue: R = a[n] && n' == n && immutable(queue, n)
    public static Object element(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[queue.start];
    }

    //Контракт:
    //Pred: n >= 1 && queue != null
    //Post: queue: R == a[n] && n' == n - 1 && immutable(queue, n')
    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;
        Object res = queue.elements[queue.start];
        queue.elements[queue.start] = null;
        queue.size--;
        queue.start = sumOnMod(queue, queue.start, -1);
        return res;
    }


    //Контракт:
    //Pred: true && queue != null
    //Post: queue: R == n && n' == n && immutable(queue, n)
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    //Контракт:
    //Pred: true && queue != null
    //Post: queue: R == (n > 0) && n' == n && immutable(queue, n)
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    //Контракт:
    //Pred: true && queue != null
    //Post: queue: n == 0
    public static void clear(ArrayQueueADT queue) {
        queue.elements = new Object[2];
        queue.end = 1;
        queue.start = 1;
        queue.size = 0;
    }


    private static void ensureCapacity(ArrayQueueADT queue,int capacity) {
        if (capacity > queue.elements.length) {
            Object[] arr = new Object[2 * queue.elements.length];
            if (queue.end == queue.elements.length - 1) {
                System.arraycopy(queue.elements, 0, arr, queue.elements.length, queue.elements.length);
                queue.end = queue.elements.length - 1;
                queue.elements = arr;
                queue.start = queue.elements.length - 1;
            } else {
                System.arraycopy(queue.elements, 0, arr, 0, queue.start + 1);
                System.arraycopy(queue.elements, queue.end + 1, arr,
                        queue.start + queue.elements.length + 1, queue.size - queue.start - 1);
                queue.end += queue.elements.length;
                queue.elements = arr;
            }
        }
    }


    //Контракт:
    //Pred: element != null && queue != null
    //Post R = n - x: x = max(i_1,...i_k): a[i_j] == element || R == -1 for i=1...n a[i] != element;
    public static int indexOf(ArrayQueueADT queue, Object element) {
        assert element != null;
        int index = 0;
        for (int i = queue.start; index != queue.size; i = sumOnMod(queue, i, -1)) {
            if (!queue.elements[i].equals(element)) {
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
    public static int lastIndexOf(ArrayQueueADT queue, Object element) {
        assert element != null;
        int index = queue.size - 1;
        for (int i = sumOnMod(queue, queue.end, 1); index != -1; i = sumOnMod(queue, i, 1)) {
            if (!queue.elements[i].equals(element)) {
                index--;
            } else {
                return index;
            }
        }
        return -1;
    }

    private static int sumOnMod(ArrayQueueADT q, int x, int val) {
        return (x + val + q.elements.length) % q.elements.length ;
    }
}
