package com.academy.fintech.kafka.grpc.export_task.v1;

import com.academy.fintech.kafka.ExportNewTaskRequest;
import com.academy.fintech.kafka.ExportServiceGrpc;
import com.academy.fintech.kafka.core.service.exporter.puller.export_task.ExportTaskCreationService;
import com.google.protobuf.Empty;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class ExportTaskController extends ExportServiceGrpc.ExportServiceImplBase {
    private final ExportTaskCreationService exportTaskCreationService;
    private final DtoMapper mapper;

    /**
     * Export task to export_task table.
     *
     * @param request request.
     * @param responseObserver observer.
     */
    @Override
    public void exportTask(ExportNewTaskRequest request, StreamObserver<Empty> responseObserver) {
        log.info("Got request: {}", request);

        try {
            exportTaskCreationService.createNewTask(
                    mapper.mapRequestToDto(request)
            );
            responseObserver.onNext(Empty.getDefaultInstance());
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error when receive new task", e);
            responseObserver.onError(e);
        }
    }
}
