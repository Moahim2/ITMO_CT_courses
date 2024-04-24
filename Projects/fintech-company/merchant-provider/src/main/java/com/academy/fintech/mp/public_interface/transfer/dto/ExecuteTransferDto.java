package com.academy.fintech.mp.public_interface.transfer.dto;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record ExecuteTransferDto(
        BigDecimal amount
) { }
