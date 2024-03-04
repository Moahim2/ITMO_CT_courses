package com.academy.fintech.pe.grpc.scoring_information.v1;

import com.academy.fintech.scoring_information.ScoringInformationRequest;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import com.academy.fintech.scoring_information.ScoringInformationServiceGrpc;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.pe.core.service.agreement.ScoringInformationCreationService;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class ScoringInformationController extends ScoringInformationServiceGrpc.ScoringInformationServiceImplBase {
    private final ScoringInformationCreationService scoringInformationCreationService;

    /**
     * End-point for calculate and find information about client's overdue and period payment.
     * It must give correct information only if:
     * <ul>
     *     <li>
     *         clientId is string in form {@code "CLI-<uuid>"}
     *     </li>
     * After executing the method, {@code observer} must contain response with:
     * <ul>
     *      <li>
     *          if ok -> {@code code = 0} and {@code periodPayment} and {@code overdueDates},
     *          where overdueDates is list of dates of overdue payments of active agreements.
     *      </li>
     *      <li> if syntax error in request -> {@code code = -1} and {@code errorMessage} </li>
     * </ul>
     *
     * @param request request.
     * @param responseObserver observer.
     */
    @Override
    public void getInformation(ScoringInformationRequest request,
                               StreamObserver<ScoringInformationResponse> responseObserver) {
        log.info("Got request: {}", request);
        try {
            responseObserver.onNext(
                    scoringInformationCreationService.createInformation(DtoMapper.createScoringDto(request))
            );
        } catch (DtoCreatingException e) {
            responseObserver.onNext(ScoringInformationResponse.newBuilder()
                    .setResponseCode(-1)
                    .setErrorMessage(e.getMessage())
                    .build());
        }
        responseObserver.onCompleted();
    }
}
