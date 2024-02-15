package info.kgeorgiy.ja.levitskii.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.*;

/**
 * The IterativeParallelism class implements the {@link ScalarIP} interface.
 * This class is able to process lists in several threads.
 * It necessarily implements operations (max, min, all, any and count)
 * taking the required number of threads as first argument.
 */
public class IterativeParallelism implements ScalarIP {
    private ParallelMapper parallelMapper = null;


    /**
     * Empty constructor.
     */
    public IterativeParallelism() {}

    /**
     * Constructor from ParallelMapper.
     * The methods of this class (with signature {@code f(int threads, List list, ...)})
     * created by this constructor work differently.
     * They don't create threads themselves, but use threads created by the {@code parallelMapper}
     * @param parallelMapper ParallelMapper.
     */
    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }


    private <T> List<List<? extends T>> getFragmentList(int realThreads, List<? extends T> list) {
        int count = list.size() / realThreads;
        int rem = list.size() % realThreads;
        ArrayList<List<? extends T>> fragments = new ArrayList<>();

        for (int i = 0; i < realThreads; i++) {
            int shift = Math.min(i, rem);
            int right = count * (i + 1) + shift;
            if (i < rem) {
                right++;
            }
            fragments.add(list.subList(count * i + shift, right));
        }
        return fragments;
    }

    private <T, E> List<E> getThreadValue(int threads, List<? extends T> list,
                                          Function<List<? extends T>, E> getValue) throws InterruptedException {
        int realThreads = Math.min(threads, list.size());
        final List<List<? extends T>> fragments = getFragmentList(realThreads, list);

        final List<E> listThreadValue;
        if (parallelMapper != null) {
            listThreadValue = parallelMapper.map(getValue, fragments);
        } else {
            listThreadValue = new ArrayList<>(Collections.nCopies(realThreads, null));
            final Thread[] arrOfThreads = new Thread[realThreads];
            for (int i = 0; i < realThreads; i++) {
                int curThread = i;
                arrOfThreads[i] = new Thread(() ->
                        listThreadValue.set(curThread, getValue.apply(fragments.get(curThread)))
                );
                arrOfThreads[i].start();
            }

            //CHANGED(HW 7)
            int i = 0;
            InterruptedException error = null;
            while (i < arrOfThreads.length) {
                try {
                    arrOfThreads[i].join();
                    i++;
                } catch (InterruptedException e) {
                    for (int k = i + 1; k < arrOfThreads.length; k++) {
                        arrOfThreads[k].interrupt();
                    }
                    if (error == null) {
                        error = e;
                    } else {
                        error.addSuppressed(e);
                    }
                }
            }
            if (error != null) {
                throw error;
            }
        }
        return listThreadValue;
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> list, Comparator<? super T> comparator) throws InterruptedException {
        Function<List<? extends T>, T> max = l -> l.stream().max(comparator).orElseThrow();
        return max.apply(getThreadValue(threads, list, max));
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> list, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, list, comparator.reversed());
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> list, Predicate<? super T> predicate) throws InterruptedException {
        return getThreadValue(threads, list, l -> l.stream().allMatch(predicate))
                .stream().allMatch(i -> i);
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> list, Predicate<? super T> predicate) throws InterruptedException {
        return !all(threads, list, predicate.negate());
    }

    @Override
    public <T> int count(int threads, List<? extends T> list, Predicate<? super T> predicate) throws InterruptedException {
        return getThreadValue(threads, list, l -> (int) l.stream().filter(predicate).count())
                .stream().reduce(0, Integer::sum);
    }
}
