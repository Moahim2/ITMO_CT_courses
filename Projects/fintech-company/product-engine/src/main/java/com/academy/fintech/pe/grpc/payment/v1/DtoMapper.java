package com.academy.fintech.pe.grpc.payment.v1;

import com.academy.fintech.application.PaymentRequest;
import com.academy.fintech.pe.grpc.payment.v1.dto.PaymentDto;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
public class DtoMapper {

    public PaymentDto mapRequestToDto(PaymentRequest request) {
        return PaymentDto.builder()
                .agreementId(request.getAgreementId())
                .amount(new BigDecimal(request.getAmount()))
                .build();
    }
}
