package com.academy.fintech.pe.grpc.agreement.v1;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.AgreementServiceGrpc.AgreementServiceImplBase;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.agreement.DisbursementResponse;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.pe.core.service.agreement.AgreementActivationService;
import com.academy.fintech.pe.core.service.agreement.AgreementCreationService;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class AgreementController extends AgreementServiceImplBase {
    private final AgreementCreationService agreementCreationService;
    private final AgreementActivationService agreementActivationService;

    /**
     * Try to create agreement from the request parameters {@code request}.
     * It must will creating agreement only if:
     * <ul>
     *     <li>
     *         clientId starts with prefix "CLI-"
     *     </li>
     *     <li>
     *         all parameters satisfy the product limits (on productCode).
     *     </li>
     * </ul>
     * Sets the status of created agreement to {@code "WAITING"}.
     * Calculates principalAmount simply as sum of disbursementAmount and originationAmount.
     * After executing the method observer must contain response with:
     * <ul>
     *      <li>if ok -> {@code code = 0} and {@code agreementId} </li>
     *      <li>if syntax error in request -> {@code code = -1} and {@code errorMessage}</li>
     *      <li>if unsuitable request conditions -> {@code code = 2} and {@code errorMessage}</li>
     * </ul>
     * @param request input request.
     * @param responseObserver observer
     */
    @Override
    public void create(AgreementRequest request, StreamObserver<AgreementResponse> responseObserver) {
        log.info("Got request: {}", request);
        try {
            responseObserver.onNext(
                    agreementCreationService.create(DtoMapper.createAgreementDto(request))
            );
        } catch (DtoCreatingException e) {
            responseObserver.onNext(AgreementResponse.newBuilder()
                    .setResponseCode(-1)
                    .setErrorMessage(e.getMessage())
                    .build()
            );
        }
        responseObserver.onCompleted();
    }

    /**
     * Try to activate agreement from the request parameters.
     * Activate is first disbursement, creating payments and schedule for it, and setting status of agreement.
     * It must will activating agreement only if:
     * <ul>
     *     <li>
     *         agreementId is string in form {@code "L-<uuid>"}
     *     </li>
     *     <li>
     *         data is string in form {@code "yyyy-mm-dd"}
     *     </li>
     *     <li>
     *         agreement (appropriate to agreementId)
     *         has already created and agreement is in the {@code "WAITING"} status now.
     *     </li>
     * </ul>
     * After executing the method, {@code observer} must contain response with:
     * <ul>
     *      <li>
     *          if ok -> {@code code = 0} and {@code paymentScheduleId}, where
     *          {@code paymentScheduleId} is string in format {@code "PS-<uuid>"}
     *      </li>
     *      <li>if syntax error in request -> {@code code = -1} and {@code errorMessage}</li>
     *      <li>if unsuitable request conditions -> {@code code = 2} and {@code errorMessage}</li>
     * </ul>
     * @param request input request.
     * @param responseObserver observer,
     */
    @Override
    public void activate(DisbursementRequest request, StreamObserver<DisbursementResponse> responseObserver) {
        log.info("Got request: {}", request);
        try {
            responseObserver.onNext(
                    agreementActivationService.activateAgreement(DtoMapper.createDisbursementDto(request))
            );
        } catch (DtoCreatingException e) {
            responseObserver.onNext(DisbursementResponse.newBuilder()
                    .setResponseCode(-1)
                    .setErrorMessage(e.getMessage())
                    .build());
        }
        responseObserver.onCompleted();
    }
}
