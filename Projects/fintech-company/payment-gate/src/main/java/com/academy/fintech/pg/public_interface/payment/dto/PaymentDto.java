package com.academy.fintech.pg.public_interface.payment.dto;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record PaymentDto(
    String agreementId,
    BigDecimal amount
) { }
