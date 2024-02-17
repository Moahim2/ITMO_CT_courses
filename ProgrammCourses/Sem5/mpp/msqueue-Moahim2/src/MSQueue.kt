import java.util.concurrent.atomic.*

/**
 * @author Ivan Levitskiy
 */
class MSQueue<E> : Queue<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        val node = Node(element)
        while (true) {
            val curTailN = tail.get()
            if (curTailN.next.compareAndSet(null, node)) {
                tail.compareAndSet(curTailN, node)
                return
            } else {
                tail.compareAndSet(curTailN, curTailN.next.get())
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val curHeadN = head.get()
            val curHeadNextN = curHeadN.next.get() ?: return null
            if (head.compareAndSet(curHeadN, curHeadNextN)) {
                val element = curHeadNextN.element
                curHeadNextN.element = null
                return element
            }
        }
    }

    // FOR TEST PURPOSE, DO NOT CHANGE IT.
    override fun validate() {
        check(tail.get().next.get() == null) {
            "At the end of the execution, `tail.next` must be `null`"
        }
        check(head.get().element == null) {
            "At the end of the execution, the dummy node shouldn't store an element"
        }
    }

    private class Node<E>(
        var element: E?
    ) {
        val next = AtomicReference<Node<E>?>(null)
    }
}
