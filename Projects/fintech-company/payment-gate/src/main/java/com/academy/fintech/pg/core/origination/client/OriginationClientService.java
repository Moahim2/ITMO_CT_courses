package com.academy.fintech.pg.core.origination.client;

import com.academy.fintech.application.NotifyingCompletionRequest;
import com.academy.fintech.pg.core.origination.client.grpc.OriginationGrpcClient;
import com.academy.fintech.pg.public_interface.transfer.dto.NotifyCompletionTransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class OriginationClientService {
    private final OriginationGrpcClient originationGrpcClient;

    /**
     * Notify origination that transfer was completed and the transfer of the completion date
     */
    public void notifyTransferCompletion(NotifyCompletionTransferDto dto) {
       originationGrpcClient.notifyTransferCompletion(mapDtoToRequest(dto));
    }

    private static NotifyingCompletionRequest mapDtoToRequest(NotifyCompletionTransferDto dto) {
        return NotifyingCompletionRequest.newBuilder()
                .setAgreementId(dto.agreementId())
                .setDate(dto.date())
                .build();
    }
}
