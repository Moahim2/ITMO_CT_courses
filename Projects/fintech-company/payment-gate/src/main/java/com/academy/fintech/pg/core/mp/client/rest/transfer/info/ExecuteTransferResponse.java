package com.academy.fintech.pg.core.mp.client.rest.transfer.info;

import lombok.Builder;

@Builder
public record ExecuteTransferResponse(
        String transferId
) { }
