package com.academy.fintech.origination.grpc.application.v1.dto;

import com.academy.fintech.application.CancelRequest;
import lombok.Builder;

@Builder
public record CancelDto(String applicationId) {

    public static CancelDto fromRequest(CancelRequest request) {
        return CancelDto.builder()
                .applicationId(request.getApplicationId())
                .build();
    }
}
