package com.academy.fintech.pg.rest.payment;

import com.academy.fintech.pg.core.service.payment.GetPaymentService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RequiredArgsConstructor
@RestController
@RequestMapping("/pg/payment")
public class PaymentController {
    private final GetPaymentService paymentService;
    private final PaymentMapper mapper;

    /**
     * End-point for getting money from client.
     */
    @PostMapping
    public void getPayment(@RequestBody PaymentRequest paymentRequest) {
        log.info("Got request: {}", paymentRequest);
        paymentService.getPaymentAndRedirectItsToPE(mapper.mapRequestToDto(paymentRequest));
    }
}
