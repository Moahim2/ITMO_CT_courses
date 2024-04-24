package com.academy.fintech.origination.core.pg.client;

import com.academy.fintech.application.TransferRequest;
import com.academy.fintech.origination.core.pg.client.grpc.PGGrpcClient;
import com.academy.fintech.origination.public_interface.transfer.dto.TransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PGClientService {
    private final PGGrpcClient pgGrpcClient;

    /**
     * Sends transfer to PG.
     */
    public void execute(TransferDto dto) {
       pgGrpcClient.execute(mapDtoToRequest(dto));
    }

    private static TransferRequest mapDtoToRequest(TransferDto dto) {
        return TransferRequest.newBuilder()
                .setAgreementId(dto.agreementId())
                .setAmount(dto.amount())
                .build();
    }
}
