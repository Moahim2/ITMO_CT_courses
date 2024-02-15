package queue;

public class MyAQADTTest {
    public static void main(String[] args) {
        ArrayQueueADT queue1 = new ArrayQueueADT();
        ArrayQueueADT queue2 = new ArrayQueueADT();
        for (int i = 0; i < 50; i++) {
            if (i > 0 && i % 5 == 0) {
                System.out.println(ArrayQueueADT.dequeue(queue1));
                System.out.println(ArrayQueueADT.dequeue(queue2));
            } else {
                ArrayQueueADT.enqueue(queue1, "a_" + i);
                ArrayQueueADT.enqueue(queue2, "b_" + i);
            }
            System.out.println(ArrayQueueADT.element(queue1));
            System.out.println(ArrayQueueADT.element(queue2));
        }
        while (!ArrayQueueADT.isEmpty(queue1) && ArrayQueueADT.size(queue1) > 10) {
            System.out.println(ArrayQueueADT.size(queue1) + " " + ArrayQueueADT.dequeue(queue1));
        }
        while (!ArrayQueueADT.isEmpty(queue2) && ArrayQueueADT.size(queue2) > 10) {
            System.out.println(ArrayQueueADT.size(queue2) + " " + ArrayQueueADT.dequeue(queue2));
        }
        ArrayQueueADT.clear(queue1);
        System.out.println(ArrayQueueADT.size(queue1));
        System.out.println(ArrayQueueADT.isEmpty(queue1));

        ArrayQueueADT.clear(queue2);
        System.out.println(ArrayQueueADT.size(queue2));
        System.out.println(ArrayQueueADT.isEmpty(queue2));
    }
}
