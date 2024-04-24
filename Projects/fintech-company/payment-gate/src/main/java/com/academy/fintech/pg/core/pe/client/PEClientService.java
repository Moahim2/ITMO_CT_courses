package com.academy.fintech.pg.core.pe.client;

import com.academy.fintech.application.PaymentRequest;
import com.academy.fintech.pg.core.pe.client.grpc.PEGrpcClient;
import com.academy.fintech.pg.public_interface.payment.dto.PaymentDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PEClientService {
    private final PEGrpcClient PEGrpcClient;

    /**
     * Sends money to PE.
     */
    public void getPayment(PaymentDto dto) {
       PEGrpcClient.getPayment(mapDtoToRequest(dto));
    }

    private static PaymentRequest mapDtoToRequest(PaymentDto dto) {
        return PaymentRequest.newBuilder()
                .setAgreementId(dto.agreementId())
                .setAmount(dto.amount().toString())
                .build();
    }
}
