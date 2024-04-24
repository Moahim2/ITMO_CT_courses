package com.academy.fintech.origination.core.kafka.client;

import com.academy.fintech.kafka.ExportNewTaskRequest;
import com.academy.fintech.origination.core.kafka.client.grpc.KafkaGrpcClient;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class KafkaClientService {
    private final KafkaGrpcClient kafkaGrpcClient;

    /**
     * Sends task to export_task db.
     */
    public void execute(String applicationId) {
       kafkaGrpcClient.execute(mapIdToRequest(applicationId));
    }

    private static ExportNewTaskRequest mapIdToRequest(String applicationId) {
        return ExportNewTaskRequest.newBuilder()
                .setType("APPLICATION")
                .setStringKey(applicationId)
                .build();
    }
}
