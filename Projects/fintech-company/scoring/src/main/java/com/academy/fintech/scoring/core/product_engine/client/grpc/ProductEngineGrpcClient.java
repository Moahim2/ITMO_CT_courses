package com.academy.fintech.scoring.core.product_engine.client.grpc;

import com.academy.fintech.scoring_information.ScoringInformationRequest;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import com.academy.fintech.scoring_information.ScoringInformationServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.StatusRuntimeException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ProductEngineGrpcClient {
    private final ScoringInformationServiceGrpc.ScoringInformationServiceBlockingStub stub;

    public ProductEngineGrpcClient(ProductEngineGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        stub = ScoringInformationServiceGrpc.newBlockingStub(channel);
    }

    public ScoringInformationResponse getScoringInformation(ScoringInformationRequest request) {
        try {
            return stub.getInformation(request);
        } catch (StatusRuntimeException e) {
            // only theoretical case because we support another error return in product-engine end-points
            log.error("Got unexpected_error from PE by request: {}", request, e);
            throw e;
        }
    }
}
