package com.academy.fintech.origination.public_interface.transfer.dto;

import lombok.Builder;

@Builder
public record TransferDto(
        String agreementId,
        String amount
) { }
