package com.academy.fintech.origination.core.service.transfer;

import com.academy.fintech.origination.core.pg.client.PGClientService;
import com.academy.fintech.origination.core.service.agreement.AgreementActivationService;
import com.academy.fintech.origination.grpc.transfer.v1.dto.NotifyCompletionTransferDto;
import com.academy.fintech.origination.public_interface.transfer.dto.TransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TransferService {
    private final PGClientService pgClientService;
    private final AgreementActivationService agreementActivationService;

    /**
     * Execute transfer to pg.
     * After calling this function, {@link #notifyTransferCompletion(NotifyCompletionTransferDto dto)}
     * is expected to be called within a few days.
     */
    public void executeTransfer(TransferDto transferDto) {
        pgClientService.execute(transferDto);
    }

    /**
     * Notify that transfer was completed and activate agreement in pe.
     */
    public void notifyTransferCompletion(NotifyCompletionTransferDto dto) {
        agreementActivationService.activate(dto);
    }
}
