package levitskiy.hw.task3;

import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Supplier;


/**
 * @author Ivan Levitskiy
 */
public class Task3 {

    /**
     * This class is implementation of the third task of hw.
     * (see tests)
     *
     * @param <T> type of products in warehouse.
     */
    public static class Warehouse<T> implements AutoCloseable {

        /**
         * Maximum of count consumers ({@value}).
         */
        private static final int COUNT_CONSUMER_THREADS = 10;



        /**
         * Maximum goods in warehouse ({@value}).
         */
        private static final int WAREHOUSE_SIZE = 100000000;



        private ExecutorService supplierExecutorService = null;
        private ExecutorService consumerExecutorService = null;

        private Supplier<List<T>> goodsSupplier;
        private Consumer<T> goodsConsumer;


        private volatile boolean flagWorking = false;


        /**
         * The warehouse start passive working.
         *
         * @param goodsSupplier conditional function of delivery of goods to the warehouse.
         * @param goodsConsumer conditional function of taking goods from the warehouse (more needed for demonstration on the console).
         */
        public void start(Supplier<List<T>> goodsSupplier, Consumer<T> goodsConsumer) {
            if (supplierExecutorService != null) {
                System.err.println("Warehouse's working has already started.");
                return;
            }
            this.goodsSupplier = goodsSupplier;
            this.goodsConsumer = goodsConsumer;

            supplierExecutorService = Executors.newSingleThreadExecutor();

            consumerExecutorService = new ThreadPoolExecutor(
                    COUNT_CONSUMER_THREADS, COUNT_CONSUMER_THREADS, 0, TimeUnit.SECONDS,
                    new ArrayBlockingQueue<>(WAREHOUSE_SIZE), Executors.defaultThreadFactory(),
                    new ThreadPoolExecutor.DiscardPolicy()
            );

            flagWorking = true;
            supplierExecutorService.execute(this::working);
        }


        private void working() {
            while (flagWorking) {
                List<T> product = goodsSupplier.get();

                if (product != null) {
                    product.forEach(p -> consumerExecutorService.execute(() -> productPickupProcessing(p)));
                }
            }
        }


        private void productPickupProcessing(T product) {
            goodsConsumer.accept(product);
        }

        /**
         * Shutdown the warehouse.
         */
        @Override
        public void close() {
            if (supplierExecutorService != null) {
                flagWorking = false;

                supplierExecutorService.shutdownNow();
                supplierExecutorService.close();

                consumerExecutorService.shutdownNow();
                consumerExecutorService.close();

                supplierExecutorService = null;
                consumerExecutorService = null;
            }
        }
    }

}
