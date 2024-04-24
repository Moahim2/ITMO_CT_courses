package com.academy.fintech.pg.rest.payment;

import com.academy.fintech.pg.public_interface.payment.dto.PaymentDto;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
public class PaymentMapper {

    public PaymentDto mapRequestToDto(PaymentRequest request) {
        return PaymentDto.builder()
                .agreementId(request.agreementId())
                .amount(new BigDecimal(request.amount()))
                .build();
    }
}
