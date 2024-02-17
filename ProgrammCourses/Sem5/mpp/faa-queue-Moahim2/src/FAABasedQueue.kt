import java.util.concurrent.atomic.*

/**
 * @author Levitskiy Ivan
 *
 * TODO: Copy the code from `FAABasedQueueSimplified`
 * TODO: and implement the infinite array on a linked list
 * TODO: of fixed-size `Segment`s.
 */
class FAABasedQueue<E> : Queue<E> {
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    private val tail : AtomicReference<Segment>
    private val head : AtomicReference<Segment>

    init {
        val dummy = AtomicReference(Segment(0))
        head = dummy
        tail = dummy
    }


    override fun enqueue(element: E) {
        while (true) {
            val curTail = tail.get()
            val i = enqIdx.getAndIncrement()
            val s = findSegment(curTail, i.toInt() / SEGMENT_SIZE)

            tail.compareAndSet(curTail.next.get(), s)

            if (s.cells.compareAndSet(i.toInt() % SEGMENT_SIZE, null, element)) {
                return
            }
        }
    }


    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (deqIdx.get() >= enqIdx.get()) return null

            val curHead = head.get()
            val i = deqIdx.getAndIncrement()
            val s = findSegment(curHead, i.toInt() / SEGMENT_SIZE)


            head.compareAndSet(curHead.next.get(), s)

            val t = i.toInt() % SEGMENT_SIZE
            if (s.cells.compareAndSet(t, null, POISONED)) {
                continue
            }
            return s.cells.getAndSet(t, null) as E
        }
    }

    private fun findSegment(start : Segment, id : Int) : Segment {
        return if (start.id.toInt() == id) {
            start
        } else {
            start.next.compareAndSet(null, Segment(start.id + 1))
            findSegment(start.next.get()!!, id)
        }
    }

}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2

private val POISONED = Any()

