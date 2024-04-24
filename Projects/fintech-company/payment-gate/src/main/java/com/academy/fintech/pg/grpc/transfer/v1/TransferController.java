package com.academy.fintech.pg.grpc.transfer.v1;

import com.academy.fintech.application.TransferRequest;
import com.academy.fintech.application.TransferServiceGrpc;
import com.academy.fintech.pg.core.service.transfer.TransferExecuteService;
import com.google.protobuf.Empty;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class TransferController extends TransferServiceGrpc.TransferServiceImplBase {
    private final TransferExecuteService transferExecuteService;
    private final DtoMapper mapper;

    /**
     * End-point for receive transfer from origination
     * and then notifies the merchant-provider about the transfer for its.
     */
    @Override
    public void execute(TransferRequest request, StreamObserver<Empty> responseObserver) {
        log.info("Got request: {}", request);

        try {
            transferExecuteService.execute(mapper.mapRequestToDto(request));
            responseObserver.onNext(Empty.getDefaultInstance());
            responseObserver.onCompleted();
        } catch (RuntimeException e) {
            log.warn(e.getMessage());
            responseObserver.onError(Status.UNKNOWN.withDescription(e.getMessage()).asRuntimeException());
        }
    }
}
