package com.academy.fintech.origination.core.kafka.client.grpc;

import com.academy.fintech.kafka.ExportNewTaskRequest;
import com.academy.fintech.kafka.ExportServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class KafkaGrpcClient {
    private final ExportServiceGrpc.ExportServiceBlockingStub stub;

    public KafkaGrpcClient(KafkaGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = ExportServiceGrpc.newBlockingStub(channel);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public void execute(ExportNewTaskRequest request) {
        stub.exportTask(request);
    }
}
