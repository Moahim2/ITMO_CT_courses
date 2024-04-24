package com.academy.fintech.mp.rest.transfer.info;

import lombok.Builder;

@Builder
public record CheckTransferResponse(
        boolean isCompleted,
        String date,
        int responseCode,
        String errorMessage
) { }
