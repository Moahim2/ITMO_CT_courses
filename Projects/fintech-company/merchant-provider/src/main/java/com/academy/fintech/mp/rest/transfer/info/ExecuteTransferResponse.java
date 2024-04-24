package com.academy.fintech.mp.rest.transfer.info;

import lombok.Builder;

@Builder
public record ExecuteTransferResponse(
        String transferId
) { }
