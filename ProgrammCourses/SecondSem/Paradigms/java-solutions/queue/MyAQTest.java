package queue;

public class MyAQTest {
    // :NOTE: * в тестах лучше использовать assert для проверки ответов на эталонность (на будущее)
    public static void main(String[] args) {
        ArrayQueue queue1 = new ArrayQueue();
        ArrayQueue queue2 = new ArrayQueue();
        for (int i = 0; i < 50; i++) {
            if (i > 0 && i % 5 == 0) {
                System.out.println(queue1.dequeue());
                System.out.println(queue2.dequeue());
            } else {
                queue1.enqueue("a_" + i);
                queue2.enqueue("b_" + i);
            }
            System.out.println(queue1.element());
            System.out.println(queue2.element());
        }
        while (!queue1.isEmpty() && queue1.size() > 10) {
            System.out.println(queue1.size() + " " + queue1.dequeue());
        }
        while (!queue2.isEmpty() && queue2.size() > 10) {
            System.out.println(queue2.size() + " " + queue2.dequeue());
        }
        queue1.clear();
        System.out.println(queue1.size());
        System.out.println(queue1.isEmpty());

        queue2.clear();
        System.out.println(queue2.size());
        System.out.println(queue2.isEmpty());
    }
}
