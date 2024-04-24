package com.academy.fintech.pg.grpc.transfer.v1.dto;

import lombok.Builder;

@Builder
public record ExecuteTransferDto(
    String agreementId,
    String amount
) { }
