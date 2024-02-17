import java.util.concurrent.atomic.*

/**
 * @author Ivan Levitskiy
 */

class Solution(val env: Environment) : Lock<Solution.Node> {
                // todo: необходимые поля (val, используем AtomicReference)
    val tail = AtomicReference<Node?>(null)


    override fun lock(): Node {
        val my = Node()
        my.locked.value = true

        val pred = tail.getAndSet(my)
        if (pred != null) {
            pred.next.value = my
            while (my.locked.value) {
                env.park()
            }
        }
        return my // вернули узел
    }

    override fun unlock(node: Node) { //node == my
        if (node.next.value == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.value == null) {
                    // pass
                }
            }
        }
        node.next.value!!.locked.value = false
        env.unpark(node.next.value!!.thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread() // запоминаем поток, которые создал узел
        val locked = AtomicReference(false)
        val next : AtomicReference<Node?> = AtomicReference(null)
                // todo: необходимые поля (val, используем AtomicReference)
    }
}
