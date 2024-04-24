package com.academy.fintech.origination.grpc.transfer.v1;

import com.academy.fintech.application.NotifyingCompletionRequest;
import com.academy.fintech.application.TransferServiceGrpc;
import com.academy.fintech.origination.core.service.transfer.TransferService;
import com.google.protobuf.Empty;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class TransferController extends TransferServiceGrpc.TransferServiceImplBase {
    private final TransferService transferService;
    private final DtoMapper mapper;

    /**
     * End-point for notifying that transfer was completed and then activating agreement in pe.
     */
    @Override
    public void notifyTransferCompletion(NotifyingCompletionRequest request, StreamObserver<Empty> responseObserver) {
        log.info("Got request: {}", request);
        log.info("Transfer was correct completed and client received the money.");
        transferService.notifyTransferCompletion(mapper.mapRequestToDto(request));
    }
}
