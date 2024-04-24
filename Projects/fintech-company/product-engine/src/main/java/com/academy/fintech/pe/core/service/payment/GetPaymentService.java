package com.academy.fintech.pe.core.service.payment;

import com.academy.fintech.pe.core.service.payment.db.account.AccountService;
import com.academy.fintech.pe.grpc.payment.v1.dto.PaymentDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class GetPaymentService {
    private final AccountService accountService;

    /**
     * Add amount from payment to main balance of agreement.
     * @param paymentDto dto.
     */
    public void getAndRegisterPayment(PaymentDto paymentDto) {
        accountService.addMoneyToMainAccount(paymentDto.amount(), paymentDto.agreementId());
    }
}
