package com.academy.fintech.origination.grpc.transfer.v1.dto;

import lombok.Builder;

@Builder
public record NotifyCompletionTransferDto(
        String agreementId,
        String date
) { }
