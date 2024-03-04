package com.academy.fintech.pe.grpc.scoring_information.v1.dto;

import com.academy.fintech.scoring_information.ScoringInformationRequest;
import lombok.Builder;

@Builder
public record InformationDto(String clientId, int disbursementAmount) {

    public static InformationDto fromRequest(ScoringInformationRequest request) {
        return InformationDto.builder()
                .clientId(request.getClientId())
                .disbursementAmount(request.getDisbursementAmount())
                .build();
    }
}
