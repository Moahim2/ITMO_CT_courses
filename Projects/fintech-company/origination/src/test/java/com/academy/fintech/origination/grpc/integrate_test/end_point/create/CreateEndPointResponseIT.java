package com.academy.fintech.origination.grpc.integrate_test.end_point.create;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.origination.grpc.integrate_test.end_point.RequestIT;
import io.grpc.Metadata;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCreateRequestAndGetId;

public interface CreateEndPointResponseIT extends RequestIT {

    @Test
    default void testCreateWithNegativeSalary() {
        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setSalary(-100)
                        .build()
        );

        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setSalary(-100000)
                        .build()
        );
    }

    @Test
    default void testCreateWithNegativeAmount() {
        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setDisbursementAmount(-100)
                        .build()
        );

        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setDisbursementAmount(-100000)
                        .build()
        );
    }

    @Test
    default void testCreateWithSyntaxIncorrectEmail() {
        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setEmail("123")
                        .build()
        );

        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setEmail("abcd.adad")
                        .build()
        );

        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setEmail("Ivan.levitskiy@")
                        .build()
        );

        checkResponseError(
                Status.INVALID_ARGUMENT,
                createRandomGoodCreateEndPointRequest()
                        .setEmail("@Levitskiy")
                        .build()
        );
    }

    @Test
    default void testCreateWithRandomGoodRequest() {
        for (int i = 0; i < 10; i++) {
            sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
        }
    }

    @Test
    default void testEmailAlreadyOccupied() {
        ApplicationRequest.Builder request = createRandomGoodCreateEndPointRequest();
        sendCreateRequestAndGetId(request.build(), getStub());

        String anotherFirstName = request.getFirstName() + "abracadabra";
        String anotherLastName = "abracadabra" + request.getLastName();


        checkResponseError(Status.RESOURCE_EXHAUSTED, request.setFirstName(anotherFirstName).build());
        checkResponseError(Status.RESOURCE_EXHAUSTED, request.setLastName(anotherLastName).build());
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    @Test
    default void testDuplicates() {
        ApplicationRequest.Builder request = createRandomGoodCreateEndPointRequest();
        String id = sendCreateRequestAndGetId(request.build(), getStub());

        for (int i = 0; i < 10; i++) {
            try {
                getStub().create(request.build());
                Assertions.fail("Must be error!");
            } catch (StatusRuntimeException e) {
                Assertions.assertEquals(Status.Code.ALREADY_EXISTS, e.getStatus().getCode());
                Assertions.assertNotNull(e.getTrailers());
                Assertions.assertEquals(
                        id,
                        e.getTrailers().get(Metadata.Key.of("application_id", Metadata.ASCII_STRING_MARSHALLER))
                );
            }
        }
    }

    @Test
    default void testNonDuplicatesButOneClient() {
        ApplicationRequest.Builder request = createRandomGoodCreateEndPointRequest()
                .setDisbursementAmount(100);

        Set<String> setId = new HashSet<>();
        String firstId = sendCreateRequestAndGetId(request.build(), getStub());
        setId.add(firstId);

        for (int i = 0; i < 10; i++) {
            String newId = sendCreateRequestAndGetId(
                    request
                            .setDisbursementAmount(request.getDisbursementAmount() + 1)
                            .build(),
                    getStub()
            );
            Assertions.assertTrue(setId.add(newId));
        }
    }


    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void checkResponseError(Status expectedStatus, ApplicationRequest request) {
        try {
            getStub().create(request);
            Assertions.fail("Must be error!");
        } catch (StatusRuntimeException e) {
            Assertions.assertEquals(expectedStatus.getCode(), e.getStatus().getCode());
        }
    }
}
