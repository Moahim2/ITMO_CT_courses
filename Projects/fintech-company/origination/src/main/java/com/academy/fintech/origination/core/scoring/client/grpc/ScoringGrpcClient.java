package com.academy.fintech.origination.core.scoring.client.grpc;

import com.academy.fintech.scoring.CheckApplicationRequest;
import com.academy.fintech.scoring.CheckApplicationResponse;
import com.academy.fintech.scoring.ScoringServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.StatusRuntimeException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ScoringGrpcClient {
    private final ScoringServiceGrpc.ScoringServiceBlockingStub stub;

    public ScoringGrpcClient(ScoringGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        stub = ScoringServiceGrpc.newBlockingStub(channel);
    }

    public CheckApplicationResponse check(CheckApplicationRequest request) throws StatusRuntimeException {
        try {
            return stub.checkApplication(request);
        } catch (StatusRuntimeException e) {
            // syntactic is potentially correct behavior, but in current real application's logic it should never be.
            log.error("Got unexpected_error from Scoring by request: {}", request, e);
            throw e;
        }
    }
}
