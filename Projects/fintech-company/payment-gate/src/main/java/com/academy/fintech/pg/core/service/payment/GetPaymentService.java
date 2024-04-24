package com.academy.fintech.pg.core.service.payment;

import com.academy.fintech.pg.core.pe.client.PEClientService;
import com.academy.fintech.pg.public_interface.payment.dto.PaymentDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class GetPaymentService {
    private final PEClientService peClientService;

    public void getPaymentAndRedirectItsToPE(PaymentDto paymentDto) {
        peClientService.getPayment(paymentDto);
    }
}
