package com.academy.fintech.pg.core.origination.client.grpc;

import com.academy.fintech.application.NotifyingCompletionRequest;
import com.academy.fintech.application.TransferServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class OriginationGrpcClient {
    private final TransferServiceGrpc.TransferServiceBlockingStub stub;

    public OriginationGrpcClient(OriginationGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = TransferServiceGrpc.newBlockingStub(channel);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public void notifyTransferCompletion(NotifyingCompletionRequest request) {
        stub.notifyTransferCompletion(request);
    }
}
