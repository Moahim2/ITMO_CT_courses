package com.academy.fintech.pg.public_interface.transfer.dto;

import lombok.Builder;

@Builder
public record NotifyCompletionTransferDto(
        String agreementId,
        String date
) { }
