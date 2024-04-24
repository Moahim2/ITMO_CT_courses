package com.academy.fintech.pe.grpc.payment.v1.dto;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record PaymentDto(
    String agreementId,
    BigDecimal amount
) { }
