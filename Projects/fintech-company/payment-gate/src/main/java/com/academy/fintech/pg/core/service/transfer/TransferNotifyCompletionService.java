package com.academy.fintech.pg.core.service.transfer;

import com.academy.fintech.pg.core.origination.client.OriginationClientService;
import com.academy.fintech.pg.public_interface.transfer.dto.NotifyCompletionTransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TransferNotifyCompletionService {
    private final OriginationClientService originationClientService;

    /**
     * Notify origination that transfer was completed.
     */
    public void notifyTransferCompletion(NotifyCompletionTransferDto dto) {
        originationClientService.notifyTransferCompletion(dto);
    }
}
