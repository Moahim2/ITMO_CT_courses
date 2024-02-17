import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 *
 * @author Ivan Levitskiy
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        if (combinerLock.compareAndSet(false, true)) {
            queue.addLast(element)
            help()
        } else {
            val ind = addNewTask(element)
            while (true) {
                if (combinerLock.compareAndSet(false, true)) {
                    if (tasksForCombiner.get(ind) !is Result<*>) {
                        queue.addLast(element)
                    }
                    tasksForCombiner.set(ind, null)
                    help()
                    break
                } else {
                    if (tasksForCombiner.get(ind) is Result<*>) {
                        tasksForCombiner.set(ind, null)
                        break
                    }
                }
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        val res : E?

        if (combinerLock.compareAndSet(false, true)) {
            res = queue.removeFirstOrNull()
            help()
        } else {
            val ind = addNewTask(Dequeue)
            while (true) {
                if (combinerLock.compareAndSet(false, true)) {
                    val t = tasksForCombiner.get(ind)
                    res = if (t is Result<*>) {
                        t.value as E?
                    } else {
                        queue.removeFirstOrNull()
                    }
                    tasksForCombiner.set(ind, null)
                    help()
                    break
                } else {
                    val t = tasksForCombiner.get(ind)
                    if (t is Result<*>) {
                        res = t.value as E?
                        tasksForCombiner.set(ind, null)
                        break
                    }
                }
            }
        }
        return res
    }

    @Suppress("UNCHECKED_CAST")
    private fun help() {
        for (i in 0..<tasksForCombiner.length()) {
            val task = tasksForCombiner[i] ?: continue

            when (task) {
                is Result<*> -> {
                    continue
                }
                Dequeue -> {
                    tasksForCombiner.set(i, Result(queue.removeFirstOrNull()))
                }
                else -> {
                    queue.addLast(task as E)
                    tasksForCombiner.set(i, Result(task))
                }
            }
        }
        combinerLock.set(false)
    }

    private fun addNewTask(newTask : Any?) : Int {
        var ind = randomCellIndex()
        while (true) {
            if (tasksForCombiner.compareAndSet(ind, null, newTask)) {
                break
            } else {
                ind = (ind + 1).mod(tasksForCombiner.length())
            }
        }
        return ind
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

private object Dequeue

private class Result<V>(
    val value: V
)