package com.academy.fintech.origination.core.pe.client.grpc;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.AgreementServiceGrpc;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.agreement.DisbursementResponse;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class PEGrpcClient {
    private final AgreementServiceGrpc.AgreementServiceBlockingStub stub;

    public PEGrpcClient(PEGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = AgreementServiceGrpc.newBlockingStub(channel);
    }

    public AgreementResponse create(AgreementRequest request) {
        return stub.create(request);
    }

    public DisbursementResponse activate(DisbursementRequest disbursementRequest) {
        return stub.activate(disbursementRequest);
    }
}
