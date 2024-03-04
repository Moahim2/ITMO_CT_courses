package com.academy.fintech.scoring.core.product_engine.client;

import com.academy.fintech.scoring.core.product_engine.client.grpc.ProductEngineGrpcClient;
import com.academy.fintech.scoring.grpc.scoring.v1.dto.CheckApplicationDto;
import com.academy.fintech.scoring_information.ScoringInformationRequest;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ProductEngineClientService {
    private final ProductEngineGrpcClient productEngineGrpcClient;

    public ScoringInformationResponse getScoringInformation(CheckApplicationDto checkApplicationDto) {
        return productEngineGrpcClient.getScoringInformation(mapDtoToRequest(checkApplicationDto));
    }

    private static ScoringInformationRequest mapDtoToRequest(CheckApplicationDto dto) {
        return ScoringInformationRequest.newBuilder()
                .setClientId(dto.clientId())
                .setDisbursementAmount(dto.disbursementAmount())
                .build();
    }
}
