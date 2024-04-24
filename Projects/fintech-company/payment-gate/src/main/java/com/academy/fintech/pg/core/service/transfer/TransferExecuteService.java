package com.academy.fintech.pg.core.service.transfer;

import com.academy.fintech.pg.core.mp.client.MPClientService;
import com.academy.fintech.pg.core.service.transfer.db.transfer.TransferService;
import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferEntity;
import com.academy.fintech.pg.grpc.transfer.v1.dto.ExecuteTransferDto;
import com.academy.fintech.pg.public_interface.transfer.dto.NotifyCompletionTransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TransferExecuteService {
    private final TransferService transferService;
    private final MPClientService mpClientService;
    private final TransferCheckService transferCheckService;

    /**
     * Executes transfer to mp and checks its executing and status in mp.
     * Notify origination if transfer is completed.
     *
     * @param executeTransferDto dto.
     * @see TransferCheckService#startCheckingTransferStatus(TransferEntity transfer)
     * @see TransferNotifyCompletionService#notifyTransferCompletion(NotifyCompletionTransferDto dto)
     */
    public void execute(ExecuteTransferDto executeTransferDto) {
        String transferId = mpClientService.executeTransfer(executeTransferDto);
        TransferEntity transfer = transferService.saveTransferAndGet(executeTransferDto, transferId);
        transferCheckService.startCheckingTransferStatus(transfer);
    }
}
