package com.academy.fintech.pe.core.kafka.client;

import com.academy.fintech.kafka.ExportNewTaskRequest;
import com.academy.fintech.pe.core.kafka.client.grpc.KafkaGrpcClient;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class KafkaClientService {
    private final KafkaGrpcClient kafkaGrpcClient;

    /**
     * Sends task to export_task db.
     */
    public void execute(String agreementId) {
       kafkaGrpcClient.execute(mapIdToRequest(agreementId));
    }

    private static ExportNewTaskRequest mapIdToRequest(String agreementId) {
        return ExportNewTaskRequest.newBuilder()
                .setType("AGREEMENT")
                .setStringKey(agreementId)
                .build();
    }
}
