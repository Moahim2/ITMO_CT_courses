package com.academy.fintech.pe.grpc.integrate_test.end_point.activate;

import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.pe.grpc.integrate_test.end_point.RequestIT;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodActivateEndPointRequest;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.sendActivateRequestAndGetId;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.sendCreateRequestAndGetId;
import static com.academy.fintech.util.TestingRequestUtil.genRandomAgreementId;

/**
 * Tests for check responses on requests in Activate-End-Point of application.
 */
public interface ActivateEndPointResponseIT extends RequestIT {

    @Transactional
    @Test
    default void testActivateWithSyntaxIncorrectAgreementId() {
        checkResponseError(createRandomGoodActivateEndPointRequest("abracadabra").build(), -1);
        checkResponseError(createRandomGoodActivateEndPointRequest("123456789").build(), -1);
    }

    @Transactional
    @Test
    default void testActivateWithSyntaxIncorrectDate() {
        String id = getStub().create(createRandomGoodCreateEndPointRequest().build()).getAgreementId();
        checkResponseError(createRandomGoodActivateEndPointRequest(id)
                .setData("2020.11.12").build(),
                -1
        );
        checkResponseError(createRandomGoodActivateEndPointRequest(id)
                        .setData("abracadabra").build(),
                -1
        );
    }

    @Transactional
    @Test
    default void testActivateWithIncorrectAgreementId() {
        checkResponseError(createRandomGoodActivateEndPointRequest(genRandomAgreementId()).build(), 2);
    }

    @Transactional
    @Test
    default void testActivateWithRandomGoodRequest() {
        for (int i = 0; i < 100; i++) {
            String agreementId = sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
            sendActivateRequestAndGetId(createRandomGoodActivateEndPointRequest(agreementId).build(), getStub());
        }
    }

    private void checkResponseError(DisbursementRequest request, int expectedErrorCode) {
        Assertions.assertEquals(expectedErrorCode, getStub().activate(request).getResponseCode());
    }
}
