package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

/**
 * @author Ivan Levitskiy
 */
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    val q = MyMultiPQ(workers, workers)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    val activeNodes = AtomicInteger(1)
    repeat(workers) {
        thread {
            while (activeNodes.get() > 0) {
                val u = q.poll() ?: continue
                for (ve in u.outgoingEdges) {
                    while (true) {
                        val v = ve.to
                        val newDistance = u.distance + ve.weight
                        val lastDistance = v.distance

                        if (newDistance < lastDistance && v.casDistance(lastDistance, newDistance)) {
                            activeNodes.incrementAndGet()
                            q.add(v)
                            break
                        } else if (newDistance >= lastDistance) {
                            break
                        }
                    }
                }
                activeNodes.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}


class MyMultiPQ(private val initCapacity : Int, count : Int) {
    private val locks = Array(count * 5) {
        ReentrantLock(false)
    }

    private val queues = Array(count * 5) {
        PriorityQueue(initCapacity, NODE_DISTANCE_COMPARATOR)
    }

    private val nullableComp = Comparator.nullsLast(NODE_DISTANCE_COMPARATOR)

    fun add(node : Node) {
        while (true) {
            val ind = randomIndex()
            val q = queues[ind]
            if (!locks[ind].tryLock()) {
                continue
            }
            q.add(node)
            locks[ind].unlock()
            return
        }
    }

    fun poll() : Node? {
        while (true) {
            val ind1 = randomIndex()
            val ind2 = randomIndex()


            val q1 = queues[ind1]
            val q2 = queues[ind2]

            val ind = if (nullableComp.compare(q1.peek(), q2.peek()) < 0) ind1 else ind2
            val q = queues[ind]
            if (!locks[ind].tryLock()) {
                continue
            }

            try {
                return q.poll()
            } finally {
                locks[ind].unlock()
            }
        }

    }

    private fun randomIndex() = ThreadLocalRandom.current().nextInt(queues.size)
}

