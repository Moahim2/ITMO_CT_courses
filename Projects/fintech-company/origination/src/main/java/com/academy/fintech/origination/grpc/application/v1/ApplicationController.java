package com.academy.fintech.origination.grpc.application.v1;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.application.ApplicationResponse;
import com.academy.fintech.application.ApplicationServiceGrpc;
import com.academy.fintech.application.CancelRequest;
import com.academy.fintech.application.CancelResponse;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.origination.core.service.application.ApplicationCancellationService;
import com.academy.fintech.origination.core.service.application.ApplicationCreationService;
import com.academy.fintech.origination.core.service.application.DuplicateException;
import com.academy.fintech.origination.core.service.application.db.client.EmailAlreadyOccupiedException;
import io.grpc.Metadata;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.devh.boot.grpc.server.service.GrpcService;

import java.util.NoSuchElementException;

@Slf4j
@GrpcService
@RequiredArgsConstructor
public class ApplicationController extends ApplicationServiceGrpc.ApplicationServiceImplBase {
    private final ApplicationCreationService applicationCreationService;
    private final ApplicationCancellationService applicationCancellationService;

    /**
     * Try to create application from the {@code request} parameters and sends a request for accept to scoring.
     * It's creating client too if the client didn't exist before and email not used by another client.
     * It must will creating application only if:
     * <ul>
     *     <li>{@code salary >= 0};</li>
     *     <li>{@code disbursementAmount >= 0};</li>
     *     <li>{@code email} syntax is correct;</li>
     *     <li>there is no such application in the {@code "NEW"} status.</li>
     * </ul>
     * Sets the status of created application to {@code "NEW"}
     * (but the status may change due to parallel interaction with scoring).
     * After executing the method observer must contain answer with:
     * <ul>
     *     <li>error with {@code Status.INVALID_ARGUMENT} if syntax error in request;</li>
     *     <li>error with {@code Status.RESOURCE_EXHAUSTED} if
     *     there is already a client with such an email but other personal data
     *     (email must be uniq);
     *     </li>
     *     <li>
     *         error with {@code Status.ALREADY_EXISTS} if a duplicate has already been created and
     *         metadata (trailers) with {@code applicationId} of this duplicate;
     *     </li>
     *     <li>response with {@code applicationId in format "APP-<uuid>"} if ok.</li>
     * </ul>
     * @param request input request.
     * @param responseObserver observer.
     * @see Status#INVALID_ARGUMENT
     * @see Status#RESOURCE_EXHAUSTED
     * @see Status#ALREADY_EXISTS
     */
    @Override
    public void create(ApplicationRequest request, StreamObserver<ApplicationResponse> responseObserver) {
        log.info("Got request: {}", request);

        try {
            responseObserver.onNext(applicationCreationService.create(DtoMapper.mapRequestToDto(request)));
            responseObserver.onCompleted();
        } catch (DtoCreatingException e) {
            formErrorResponseAndSend(responseObserver, Status.INVALID_ARGUMENT, e);
        } catch (EmailAlreadyOccupiedException e) {
            formErrorResponseAndSend(responseObserver, Status.RESOURCE_EXHAUSTED, e);
        } catch (DuplicateException e) {
            Metadata trailers = new Metadata();
            Metadata.Key<String> key = Metadata.Key.of("application_id", Metadata.ASCII_STRING_MARSHALLER);
            trailers.put(key, e.getApplicationId());

            formErrorResponseAndSend(responseObserver, Status.ALREADY_EXISTS, e, trailers);
        }
    }

    /**
     * Try to cancel application with id from the {@code request} parameters.
     * It will close application only if:
     * <ul>
     *     <li>{@code applicationId} is string in format {@code "APP-<uuid>"};</li>
     *     <li>application (with requested {@code applicationId}) has already existed;</li>
     *     <li>the application has not yet been accepted and has not yet been closed.</li>
     * </ul>
     * Sets the status of required application to {@code "CLOSED"}).
     * After executing the method observer must contain answer with:
     * <ul>
     *     <li>error with {@code Status.INVALID_ARGUMENT} if syntax error in request;</li>
     *     <li>error with {@code Status.NOT_FOUND} if there is no such application;</li>
     *     <li>response with {@code flag == false} if application has already been accepted or closed;</li>
     *     <li>response with {@code flag == true} if ok.</li>
     * </ul>
     * @param request input request.
     * @param responseObserver observer.
     * @see Status#INVALID_ARGUMENT
     * @see Status#NOT_FOUND
     */
    @Override
    public void cancel(CancelRequest request, StreamObserver<CancelResponse> responseObserver) {
        log.info("Got request: {}", request);

        try {
            responseObserver.onNext(applicationCancellationService.cancel(DtoMapper.mapRequestToDto(request)));
            responseObserver.onCompleted();
        } catch (DtoCreatingException e) {
            formErrorResponseAndSend(responseObserver, Status.INVALID_ARGUMENT, e);
        } catch (NoSuchElementException e) {
            formErrorResponseAndSend(responseObserver, Status.NOT_FOUND, e);
        }
    }

    private static void formErrorResponseAndSend(StreamObserver<?> responseObserver, Status status, Exception e) {
        formErrorResponseAndSend(responseObserver, status, e, null);
    }

    private static void formErrorResponseAndSend(StreamObserver<?> responseObserver,
                                                 Status status,
                                                 Exception e,
                                                 Metadata trailers) {
        responseObserver.onError(status
                .withDescription(e.getMessage())
                .asRuntimeException(trailers)
        );
    }
}
