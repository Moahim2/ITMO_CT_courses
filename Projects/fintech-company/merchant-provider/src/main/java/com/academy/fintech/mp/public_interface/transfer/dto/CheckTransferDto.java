package com.academy.fintech.mp.public_interface.transfer.dto;

import lombok.Builder;

@Builder
public record CheckTransferDto(
        String transferId,
        boolean isCompleted,
        String date
) { }
