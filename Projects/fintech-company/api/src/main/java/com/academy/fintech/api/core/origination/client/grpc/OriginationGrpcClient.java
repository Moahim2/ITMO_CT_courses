package com.academy.fintech.api.core.origination.client.grpc;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.application.ApplicationResponse;
import com.academy.fintech.application.ApplicationServiceGrpc;
import com.academy.fintech.application.ApplicationServiceGrpc.ApplicationServiceBlockingStub;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Status;
import io.grpc.StatusException;
import io.grpc.StatusRuntimeException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class OriginationGrpcClient {
    private final ApplicationServiceBlockingStub stub;

    public OriginationGrpcClient(OriginationGrpcClientProperty property) {
        Channel channel = ManagedChannelBuilder.forAddress(property.host(), property.port()).usePlaintext().build();
        this.stub = ApplicationServiceGrpc.newBlockingStub(channel);
    }

    /**
     * Sends a request to the origination service to create application.
     *
     * @param applicationRequest request.
     * @return response with {@code applicationId}.
     * @throws StatusRuntimeException if
     * request wasn't syntax correct for origination service.
     * @throws StatusException if origination has duplicate of this application.
     */
    public ApplicationResponse createApplication(ApplicationRequest applicationRequest)
            throws StatusRuntimeException, StatusException {
        try {
            return stub.create(applicationRequest);
        } catch (StatusRuntimeException e) {
            log.error("Got error from Origination by request: {}", applicationRequest, e);
            if (e.getStatus().getCode() == Status.Code.ALREADY_EXISTS) {
                throw createCheckedStatusException(e);
            } else {
                throw e;
            }
        }
    }

    /**
     * Convert {@code StatusRuntimeException} to {@code StatusException}.
     *
     * @param e exception.
     * @return new exception, which contains all the information from old exception.
     */
    private static StatusException createCheckedStatusException(StatusRuntimeException e) {
        return e.getStatus().asException(e.getTrailers());
    }
}
