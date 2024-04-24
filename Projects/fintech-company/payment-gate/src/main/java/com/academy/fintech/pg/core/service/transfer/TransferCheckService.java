package com.academy.fintech.pg.core.service.transfer;

import com.academy.fintech.pg.core.mp.client.MPClientService;
import com.academy.fintech.pg.core.mp.client.rest.transfer.excep.TransferCheckException;
import com.academy.fintech.pg.core.service.transfer.db.transfer.TransferService;
import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferEntity;
import com.academy.fintech.pg.public_interface.transfer.dto.CheckTransferDto;
import com.academy.fintech.pg.public_interface.transfer.dto.NotifyCompletionTransferDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class TransferCheckService {
    @Value("${properties.service.transfer.check.initial-delay}")
    private long initialDelay;
    @Value("${properties.service.transfer.check.ground-exponent}")
    private long groundExponent;
    @Value("${properties.service.transfer.check.full-max-check-time}")
    private long fullMaxCheckTime;
    @Value("${properties.service.transfer.check.time-type}")
    private TimeUnit timeType;

    private final TransferService transferService;
    private final MPClientService mpClientService;
    private final TransferNotifyCompletionService transferNotifyCompletionService;
    private final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(100);
    private final Set<String> idsOfCheckingTransfers = ConcurrentHashMap.newKeySet();

    /**
     * Runs a periodic check of the transfer status in the service,
     * records changes in its status and reports its successful completion to origination.
     */
    public void startCheckingTransferStatus(TransferEntity transfer) {
        if (idsOfCheckingTransfers.contains(transfer.getId())) {
            return;
        }
        formNewScheduledRequestForChecking(initialDelay, 0, mapEntityToDto(transfer));
        idsOfCheckingTransfers.add(transfer.getAgreementId());
    }

    /**
     * Sometimes a function that starts resumes checking for unchecked transfers (due to server shutdown, etc.).
     */
    @Async
    @Scheduled(
            fixedRate = 1,
            initialDelay = 1,
            timeUnit = TimeUnit.DAYS
    )
    protected void startCheckingAllOldOrUnregisteredTransfers() {
        for (TransferEntity transfer : transferService.getAllExecutingTransfers()) {
            startCheckingTransferStatus(transfer);
        }
    }

    private void formNewScheduledRequestForChecking(long delay, long previousSumTime, CheckTransferDto dto) {
        executorService.schedule(new CheckTransferTask(dto, delay, previousSumTime), delay, timeType);
    }

    private long changeDelayFunction(long previousDelay) {
        return previousDelay * groundExponent;
    }

    private static CheckTransferDto mapEntityToDto(TransferEntity transferEntity) {
        return CheckTransferDto.builder()
                .transferId(transferEntity.getId())
                .build();
    }

    /**
     * The task for performing one deferred check transfer status in MP (and planning the next one if necessary).
     */
    private final class CheckTransferTask implements Runnable {
        private final long delay;
        private final long curSumTime;
        private final CheckTransferDto dto;

        private CheckTransferTask(CheckTransferDto dto, long delay, long previousSumTime) {
            this.delay = delay;
            this.dto = dto;
            this.curSumTime = previousSumTime + delay;
        }

        /**
         * Check transfer completion in mp and
         * set its status to completed and notify origination if transfer was completed
         * else schedule new check using formula {@link #changeDelayFunction(long previousDelay)} for delay.
         * Checking process will be finish and status will be set to rejected.
         * if summary transfer time greater than {@link #fullMaxCheckTime}.
         */
        @Override
        public void run() {
            CheckTransferDto responseDto;
            try {
                responseDto = mpClientService.checkTransfer(dto);
            } catch (TransferCheckException e) {
                nextScheduleRequest();
                return;
            }
            if (!responseDto.isCompleted()) {
                nextScheduleRequest();
                return;
            }

            String agreementId = transferService
                    .setStatusToCompletedAndGetAgreementId(dto.transferId())
                    .orElseThrow(() -> {
                        log.warn("Unexpected server error, unregistered transfer in processing.");
                        return new NoSuchElementException(dto.transferId());
                    });
            transferNotifyCompletionService.notifyTransferCompletion(NotifyCompletionTransferDto.builder()
                    .agreementId(agreementId)
                    .date(responseDto.date())
                    .build()
            );
            idsOfCheckingTransfers.remove(dto.transferId());
        }

        private void nextScheduleRequest() {
            if (curSumTime >= fullMaxCheckTime) {
                transferService.setStatusToRejected(dto.transferId());
                idsOfCheckingTransfers.remove(dto.transferId());
                return;
            }
            formNewScheduledRequestForChecking(changeDelayFunction(delay), curSumTime, dto);
        }
    }
}
