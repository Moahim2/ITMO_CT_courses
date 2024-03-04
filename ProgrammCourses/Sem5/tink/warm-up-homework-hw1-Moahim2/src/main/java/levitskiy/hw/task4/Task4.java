package levitskiy.hw.task4;

import java.util.List;
import java.util.stream.IntStream;


/**
 * @author Ivan Levitskiy
 */
public class Task4 {

    private static final class BlockingPrinter {
        private int currentWorkingThread = 1;
        private String result = "";


        private synchronized void printNum(int numberOfThread) {
            while (currentWorkingThread != numberOfThread) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
            System.out.print(numberOfThread);
            result += numberOfThread;
            
            currentWorkingThread = (currentWorkingThread % 3) + 1;
            notifyAll();
        }
    }

    /**
     * The method from the fourth number of hw.
     * <p>
     * The method outputs the numbers to the console
     * and writes them to the resulting variable and return it,
     * because it is not clear from the task what is required.
     *
     * @return String "123123123123123123".
     * @throws InterruptedException if current thread was interrupted.
     */
    public String printAndReturnStr() throws InterruptedException {
        final BlockingPrinter blockingPrinter = new BlockingPrinter();

        List<Thread> threads = IntStream.range(1, 4)
                .mapToObj(numberOfThread ->
                        new Thread(() ->
                                IntStream.range(0, 6)
                                        .forEach(unused ->
                                                blockingPrinter.printNum(numberOfThread))))
                .toList();

        threads.forEach(Thread::start);


        InterruptedException error = null;
        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                error = e;
            }
        }
        if (error != null) {
            throw error;
        }


        return blockingPrinter.result;
    }



    /**
     * Example of work task4 in console.
     */
    public static void main(String[] args) {
        try {
            new Task4().printAndReturnStr();
        } catch (InterruptedException e) {
            System.out.println("Interrupted exception!");
        }
    }
}
