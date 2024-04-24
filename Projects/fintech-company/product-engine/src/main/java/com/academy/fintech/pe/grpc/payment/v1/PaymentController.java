package com.academy.fintech.pe.grpc.payment.v1;

import com.academy.fintech.application.PaymentRequest;
import com.academy.fintech.application.PaymentResponse;
import com.academy.fintech.application.PaymentServiceGrpc;
import com.academy.fintech.pe.core.service.payment.GetPaymentService;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class PaymentController extends PaymentServiceGrpc.PaymentServiceImplBase {
    private final GetPaymentService getPaymentService;
    private final DtoMapper mapper;

    /**
     * According to this end-point, the main account of the agreement gets the client's money.
     */
    @Override
    public void get(PaymentRequest request, StreamObserver<PaymentResponse> responseObserver) {
        log.info("Got request: {}", request);

        getPaymentService.getAndRegisterPayment(mapper.mapRequestToDto(request));
        responseObserver.onNext(PaymentResponse.newBuilder().build());
        responseObserver.onCompleted();
    }
}
