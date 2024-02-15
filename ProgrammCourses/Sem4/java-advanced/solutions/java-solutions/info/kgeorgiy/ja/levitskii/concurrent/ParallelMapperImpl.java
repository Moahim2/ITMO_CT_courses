package info.kgeorgiy.ja.levitskii.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;


/**
 * The ParallelMapperImpl class implements the {@link ParallelMapper}.
 * This class is used to safely parallelize a task into smaller subtasks
 * using internally created threads.
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threadList;
    private final Deque<Runnable> taskQueue = new ArrayDeque<>();


    private static final class Counter {
        int count = 0;

        public synchronized void inc() {
            count++;
            notify();
        }

        public synchronized void waitingAll(int stopCount) throws InterruptedException {
            while (count != stopCount) {
                wait();
            }
        }
    }


    private static final class MyAtomicRuntimeException {
        RuntimeException error = null;

        public synchronized void addException(RuntimeException error) {
            if (this.error == null) {
                this.error = error;
            } else {
                this.error.addSuppressed(error);
            }
        }

        public synchronized void throwError() {
            if (error != null) {
                throw error;
            }
        }

    }

    /**
     * This constructor creates {@code threads} worker threads that can be used for parallelization.
     * @param threads count of threads.
     */
    public ParallelMapperImpl(int threads) {
        threadList = Stream.generate(() ->
                new Thread(() -> {
                    try {
                        while (!Thread.interrupted()) {
                            final Runnable task;
                            synchronized (taskQueue) {
                                while (taskQueue.isEmpty()) {
                                    taskQueue.wait();
                                }
                                task = taskQueue.pop();
                            }
                            task.run();
                        }
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                })).limit(threads).toList();
        for (Thread thread : threadList) {
            thread.start();
        }
    }



    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args)
            throws InterruptedException {
        if (f == null) {
            throw new IllegalArgumentException("Error, f is null.");
        }
        if (args == null) {
            throw new IllegalArgumentException("Error, args is null.");
        }
        final List<R> results = new ArrayList<>(Collections.nCopies(args.size(), null));
        final Counter counter = new Counter();

        final MyAtomicRuntimeException error = new MyAtomicRuntimeException();

        for (int i = 0; i < results.size(); i++) {
            int curArg = i;
            synchronized (taskQueue) {
                taskQueue.add(() -> {
                    try {
                        results.set(curArg, f.apply(args.get(curArg)));
                    } catch (RuntimeException e) {
                        error.addException(e);
                    }
                    counter.inc();
                });
                taskQueue.notify();
            }
        }

        counter.waitingAll(results.size());
        error.throwError();
        return results;
    }


    @Override
    public void close() {
        for (Thread thread : threadList) {
            thread.interrupt();
        }
        int i = 0;
        boolean f = false;
        while (i < threadList.size()) {
            try {
                threadList.get(i).join();
                i++;
            } catch (InterruptedException e) {
                //Thread.currentThread().interrupt(); changed
                f = true;
            }
        }
        if (f) Thread.currentThread().interrupt();
    }

}
