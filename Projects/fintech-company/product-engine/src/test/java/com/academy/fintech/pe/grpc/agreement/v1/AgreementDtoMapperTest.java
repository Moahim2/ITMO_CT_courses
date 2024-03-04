package com.academy.fintech.pe.grpc.agreement.v1;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.BigDecimalValue;
import com.google.protobuf.ByteString;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.pe.grpc.agreement.TestingDtoMapperUtil.checkDtoCreatingException;

public class AgreementDtoMapperTest {

    @Test
    public void testSyntaxIncorrectClientId() {
        testDtoException(createRandomGoodCreateEndPointRequest().setClientId("abracadabra"));
        testDtoException(createRandomGoodCreateEndPointRequest().setClientId("123456789"));
    }

    @Test
    public void testSyntaxIncorrectBDValue() {
        testDtoException(createRandomGoodCreateEndPointRequest()
                .setOriginationAmount(BigDecimalValue.newBuilder()
                        .setPrecision(10)
                        .setScale(5)
                        .setValue(ByteString.EMPTY)));
        testDtoException(createRandomGoodCreateEndPointRequest()
                .setOriginationAmount(BigDecimalValue.newBuilder()
                        .setPrecision(-500)
                        .setScale(5)
                        .setValue(ByteString.copyFrom("abracadabra".getBytes()))));
    }

    @Test
    public void testGoodRequest() {
        for (int i = 0; i < 100; i++) {
            Assertions.assertDoesNotThrow(() ->
                    DtoMapper.createAgreementDto(createRandomGoodCreateEndPointRequest().build()));
        }
    }

    private void testDtoException(AgreementRequest.Builder agreementBuilder) {
        checkDtoCreatingException(() -> DtoMapper.createAgreementDto(agreementBuilder.build()));
    }
}
