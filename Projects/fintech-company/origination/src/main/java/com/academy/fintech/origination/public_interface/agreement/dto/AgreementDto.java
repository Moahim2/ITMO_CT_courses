package com.academy.fintech.origination.public_interface.agreement.dto;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record AgreementDto(
        String clientId,
        int loanTerm,
        BigDecimal disbursementAmount,
        BigDecimal originationAmount,
        BigDecimal interest,
        String productName
) { }
