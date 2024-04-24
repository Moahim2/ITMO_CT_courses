package com.academy.fintech.mp.rest.transfer.info;

import lombok.Builder;

@Builder
public record ExecuteTransferRequest(
    String amount
) { }
