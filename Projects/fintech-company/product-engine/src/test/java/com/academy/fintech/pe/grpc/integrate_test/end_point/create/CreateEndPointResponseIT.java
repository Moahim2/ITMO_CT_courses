package com.academy.fintech.pe.grpc.integrate_test.end_point.create;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.BigDecimalValue;
import com.academy.fintech.pe.grpc.integrate_test.end_point.RequestIT;
import com.academy.fintech.pe.grpc.TestingAgreementRequestUtil;
import com.google.protobuf.ByteString;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.sendCreateRequestAndGetId;

/**
 * Tests for check responses on requests in Create-End-Point of application.
 */
public interface CreateEndPointResponseIT extends RequestIT {

    @Transactional
    @Test
    default void testCreateWithSyntaxIncorrectClientId() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setClientId("123")
                        .build(),
                -1
        );
    }

    @Transactional
    @Test
    default void testCreateWithSyntaxIncorrectBigDecimalValue() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setOriginationAmount(BigDecimalValue.newBuilder()
                                .setPrecision(10)
                                .setScale(5)
                                .setValue(ByteString.EMPTY)
                                .build())
                        .build(),
                -1
        );
    }

    @Transactional
    @Test
    default void testCreateWithIncorrectLoanTerm() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setLoanTerm(0)
                        .build(),
                2
        );
    }

    @Transactional
    @Test
    default void testCreateWithIncorrectDisbursementAmount() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setDisbursementAmount(TestingAgreementRequestUtil.toBDValue("5000000000"))
                        .build(),
                2
        );
    }

    @Transactional
    @Test
    default void testCreateWithIncorrectOriginationAmount() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setOriginationAmount(TestingAgreementRequestUtil.toBDValue("100"))
                        .build(),
                2
        );
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setOriginationAmount(TestingAgreementRequestUtil.toBDValue("1000000"))
                        .build(),
                2
        );
    }


    @Transactional
    @Test
    default void testCreateWithIncorrectInterest() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setInterest(TestingAgreementRequestUtil.toBDValue("7"))
                        .build(),
                2
        );
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setInterest(TestingAgreementRequestUtil.toBDValue("16"))
                        .build(),
                2
        );
    }

    @Transactional
    @Test
    default void testCreateWithIncorrectProductCode() {
        checkResponseError(
                createRandomGoodCreateEndPointRequest()
                        .setProductName("abracadabra")
                        .build(),
                2
        );
    }

    @Transactional
    @Test
    default void testCreateWithRandomGoodRequest() {
        for (int i = 0; i < 100; i++) {
            sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
        }
    }

    private void checkResponseError(AgreementRequest request, int expectedErrorCode) {
        Assertions.assertEquals(expectedErrorCode, getStub().create(request).getResponseCode());
    }
}
