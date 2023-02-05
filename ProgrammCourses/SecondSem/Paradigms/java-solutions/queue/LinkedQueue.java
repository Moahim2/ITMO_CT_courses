package queue;

import java.util.function.Predicate;

public class LinkedQueue extends AbstractQueue {
    private Node head;
    private Node tail;

    @Override
    protected void enqueueImpl(Object element) {
        tail = new Node(element, tail, tail);
        tail.prev = tail;
        if (size == 0) {
            tail.next = tail;
            head = tail;
        } else {
            tail.next.prev = tail;
        }
    }

    @Override
    protected Object elementImpl() {
        return head.value;
    }

    @Override
    public void dequeueImpl() {
        head = head.prev;
        head.next = head;
    }

    @Override
    protected void clearImpl() {
        head.prev = head;
        head.next = head;
        tail = head;
    }

    // :NOTE: избавится от копипасты в методах indexIfImpl и lastIndexIfImpl
    @Override
    protected int indexIfImpl(Predicate<Object> predicate, int index) {
        Node current = head;
        while (index != size && !predicate.test(current.value)) {
            current = current.prev;
            index++;
        }
        if (index == size) {
            return -1;
        }
        return index;
    }

    @Override
    protected int lastIndexIfImpl(Predicate<Object> predicate, int index) {
        Node current = tail;
        while (index != -1 && !predicate.test(current.value)) {
            current = current.next;
            index--;
        }
        return index;
    }

    private static class Node {
        private final Object value;
        private Node next;
        private Node prev;
        public Node(Object value, Node next, Node prev) {
            assert value != null;
            this.value = value;
            this.next = next;
            this.prev = prev;
        }
    }
}
