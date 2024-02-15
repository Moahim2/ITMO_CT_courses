package queue;

public class MyAQMTest {


    public static void main(String[] args) {
        for (int i = 0; i < 50; i++) {
            if (i > 0 && i % 5 == 0) {
                System.out.println(ArrayQueueModule.dequeue());
            } else {
                ArrayQueueModule.enqueue("a_" + i);
            }
            System.out.println(ArrayQueueModule.element());
        }
        while (!ArrayQueueModule.isEmpty() && ArrayQueueModule.size() > 10) {
            System.out.println(ArrayQueueModule.size() + " " + ArrayQueueModule.dequeue());
        }
        ArrayQueueModule.clear();
        System.out.println(ArrayQueueModule.size());
        System.out.println(ArrayQueueModule.isEmpty());
    }
}