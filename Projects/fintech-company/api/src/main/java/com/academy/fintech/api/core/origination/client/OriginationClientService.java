package com.academy.fintech.api.core.origination.client;

import com.academy.fintech.api.core.origination.client.grpc.OriginationGrpcClient;
import com.academy.fintech.api.public_interface.application.dto.ApplicationDto;
import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.application.ApplicationResponse;
import io.grpc.Metadata;
import io.grpc.StatusException;
import io.grpc.StatusRuntimeException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class OriginationClientService {

    private final OriginationGrpcClient originationGrpcClient;

    /**
     * Forms and sends a request to the origination service to create application.
     *
     * @param applicationDto dto.
     * @return id of created application if
     * application was correct created in origination or origination has duplicate this application.
     * @throws StatusRuntimeException if
     * request created from {@code dto} wasn't syntax correct for origination service.
     */
    public String createApplication(ApplicationDto applicationDto) throws StatusRuntimeException {
        ApplicationRequest request = mapDtoToRequest(applicationDto);

        String applicationId;
        try {
            ApplicationResponse response = originationGrpcClient.createApplication(request);
            applicationId = response.getApplicationId();
        } catch (StatusException e) {
            applicationId = e.getTrailers().get(Metadata.Key.of("application_id", Metadata.ASCII_STRING_MARSHALLER));
        }
        return applicationId;
    }

    private static ApplicationRequest mapDtoToRequest(ApplicationDto applicationDto) {
        return ApplicationRequest.newBuilder()
                .setFirstName(applicationDto.firstName())
                .setLastName(applicationDto.lastName())
                .setEmail(applicationDto.email())
                .setSalary(applicationDto.salary())
                .setDisbursementAmount(applicationDto.amount())
                .build();
    }
}
