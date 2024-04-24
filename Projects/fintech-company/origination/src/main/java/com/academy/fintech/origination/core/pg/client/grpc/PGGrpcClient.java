package com.academy.fintech.origination.core.pg.client.grpc;

import com.academy.fintech.application.TransferRequest;
import com.academy.fintech.application.TransferServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class PGGrpcClient {
    private final TransferServiceGrpc.TransferServiceBlockingStub stub;

    public PGGrpcClient(PGGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = TransferServiceGrpc.newBlockingStub(channel);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public void execute(TransferRequest request) {
        stub.execute(request);
    }
}
