package com.academy.fintech.pg.rest.payment;

import lombok.Builder;

@Builder
public record PaymentRequest(
        String agreementId,
        String amount
) { }
