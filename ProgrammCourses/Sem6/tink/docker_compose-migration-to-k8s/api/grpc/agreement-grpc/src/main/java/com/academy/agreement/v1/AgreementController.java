package com.academy.agreement.v1;

import com.academy.public_interface.agreement.v1.AgreementResponseDto;
import com.academy.public_interface.agreement.v1.AgreementServiceV1;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import org.lognet.springboot.grpc.GRpcService;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;
import reactor.core.publisher.Flux;

@GRpcService
@RequiredArgsConstructor
public class AgreementController extends AgreementServiceGrpc.AgreementServiceImplBase {

    private final AgreementServiceV1 agreementServiceV1;

    @Override
    public void create(AgreementRequest request, StreamObserver<AgreementResponse> responseObserver) {
        AgreementResponseDto agreementResponseDto = agreementServiceV1.create(request.getStatus());

        responseObserver.onNext(
                AgreementResponse.newBuilder()
                        .setId(agreementResponseDto.id())
                        .setStatus(agreementResponseDto.status())
                        .build()
        );
        responseObserver.onCompleted();
    }

    @Override
    public void get(AgreementRequest request, StreamObserver<AgreementResponse> responseObserver) {
        Flux<AgreementResponseDto> flux = agreementServiceV1.get(request.getStatus());

        flux.subscribe(new Subscriber<>() {
            @Override
            public void onSubscribe(Subscription subscription) {

            }

            @Override
            public void onNext(AgreementResponseDto dto) {
                responseObserver.onNext(
                        AgreementResponse.newBuilder()
                                .setId(dto.id())
                                .setStatus(dto.status())
                                .build()
                );
            }

            @Override
            public void onError(Throwable throwable) {
                responseObserver.onError(throwable);
            }

            @Override
            public void onComplete() {
                responseObserver.onCompleted();
            }
        });

    }

}
