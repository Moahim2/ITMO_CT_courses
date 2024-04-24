package com.academy.fintech.pg.core.pe.client.grpc;

import com.academy.fintech.application.PaymentRequest;
import com.academy.fintech.application.PaymentServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class PEGrpcClient {
    private final PaymentServiceGrpc.PaymentServiceBlockingStub stub;

    public PEGrpcClient(PEGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = PaymentServiceGrpc.newBlockingStub(channel);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public void getPayment(PaymentRequest request) {
        stub.get(request);
    }
}
