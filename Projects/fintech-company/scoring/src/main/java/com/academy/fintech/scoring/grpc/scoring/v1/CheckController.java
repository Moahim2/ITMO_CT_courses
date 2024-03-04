package com.academy.fintech.scoring.grpc.scoring.v1;

import com.academy.fintech.scoring.CheckApplicationRequest;
import com.academy.fintech.scoring.CheckApplicationResponse;
import com.academy.fintech.scoring.ScoringServiceGrpc;
import com.academy.fintech.scoring.core.ServiceInteractionError;
import com.academy.fintech.scoring.core.service.check_application.ScoringCheckService;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class CheckController extends ScoringServiceGrpc.ScoringServiceImplBase {
    private final ScoringCheckService scoringCheckService;

    /**
     * Try to check application characteristics and calculate score for application (on data from request).
     * Does not save the calculated information anywhere.
     * After executing the method observer must contain answer with:
     * <ul>
     *      <li>error with {@code Status.UNKNOWN} if was unexpected errors with connection or other</li>
     *      <li>response with score result if ok</li>
     * </ul>
     * @param request request.
     * @param responseObserver observer.
     */
    @Override
    public void checkApplication(CheckApplicationRequest request,
                                 StreamObserver<CheckApplicationResponse> responseObserver) {
        log.info("Got request: {}", request);
        try {
            responseObserver.onNext(
                    scoringCheckService.checkApplication(DtoMapper.mapToDto(request))
            );
            responseObserver.onCompleted();
        } catch (ServiceInteractionError e) {
            // if connection refused with PE
            responseObserver.onError(Status.UNKNOWN
                    .withDescription(e.getMessage())
                    .asRuntimeException()
            );
        }
    }
}
