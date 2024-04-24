package com.academy.fintech.mp.core.service.transfer;

import com.academy.fintech.mp.core.service.transfer.exception.TransferException;
import com.academy.fintech.mp.public_interface.transfer.dto.CheckTransferDto;
import com.academy.fintech.mp.public_interface.transfer.dto.ExecuteTransferDto;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Mock-service for pseudo-imitation of work with real cache.
 */
@Service
public class TransferService {
    private static final String ID_PREFIX = "tr";

    @Value("${properties.service.transfer.execute.min-transfer-delay-time}")
    private long minTransferDelayTime;
    @Value("${properties.service.transfer.execute.max-transfer-delay-time}")
    private long maxTransferDelayTime;
    @Value("${properties.service.transfer.execute.time-type}")
    private TimeUnit timeType;

    private final Map<String, Future<CheckTransferDto>> resultMap = new ConcurrentHashMap<>();
    private final AtomicLong nextIdNumber = new AtomicLong();
    private final ExecutorService executorService;
    private final Random random;

    public TransferService(@Value("${properties.service.transfer.execute.executor.core-pool-size}") int corePoolSize,
                           @Value("${properties.service.transfer.execute.executor.max-pool-size}") int maxPoolSize,
                           @Value("${properties.service.transfer.execute.executor.keep-alive-time}") long keepAliveTime,
                           @Value("${properties.service.transfer.execute.random.random-seed}") long randomSeed) {
        executorService = new ThreadPoolExecutor(
                corePoolSize, maxPoolSize, keepAliveTime, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(), Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.CallerRunsPolicy()
        );
        random = new Random(randomSeed);
    }

    /**
     * Start sub-process with executing of transfer.
     *
     * @param executeTransferDto dto
     * @return id of new transfer.
     */
    public String executeTransfer(ExecuteTransferDto executeTransferDto) {
        String id = makeNextId();
        resultMap.put(id, executorService.submit(new TransferTask(executeTransferDto)));
        return id;
    }

    /**
     * Check completion of this transfer.
     *
     * @param checkTransferDto dto.
     * @return dto with flag of transfer completion status and date of completion.
     * @throws TransferException if process with this id of transfer wasn't found.
     */
    public CheckTransferDto checkTransfer(CheckTransferDto checkTransferDto)
            throws ExecutionException, InterruptedException {
        Future<CheckTransferDto> result = resultMap.get(checkTransferDto.transferId());
        if (result == null) {
            throw new TransferException("Transfer with this id isn't found.");
        }
        if (result.isDone()) {
            return result.get();
        }
        return buildDtoWithNotCompletedFlag();
    }

    private String makeNextId() {
        return ID_PREFIX + nextIdNumber.getAndIncrement();
    }

    /**
     * Pseudo-imitation of work with random time.
     *
     * @param dto parameters for working.
     */
    private CheckTransferDto performTransfer(ExecuteTransferDto dto) {
        // any proposed job with dto (with real money)
        //
        //
        try {
            Thread.sleep(
                    timeType.toMillis(
                            random.nextLong(minTransferDelayTime, maxTransferDelayTime)
                    )
            );
            return CheckTransferDto.builder()
                    .isCompleted(true)
                    .date(LocalDate.now().toString())
                    .build();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        return buildDtoWithNotCompletedFlag();
    }

    /**
     * Task with transfer working.
     */
    private class TransferTask implements Callable<CheckTransferDto> {
        private final ExecuteTransferDto transfer;

        private TransferTask(ExecuteTransferDto transfer) {
            this.transfer = transfer;
        }

        @Override
        public CheckTransferDto call() {
            return performTransfer(this.transfer);
        }
    }

    private static CheckTransferDto buildDtoWithNotCompletedFlag() {
        return CheckTransferDto.builder()
                .isCompleted(false)
                .build();
    }
}
