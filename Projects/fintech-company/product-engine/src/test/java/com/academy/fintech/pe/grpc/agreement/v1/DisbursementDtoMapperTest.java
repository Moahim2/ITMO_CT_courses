package com.academy.fintech.pe.grpc.agreement.v1;

import com.academy.fintech.agreement.DisbursementRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodActivateEndPointRequest;
import static com.academy.fintech.pe.grpc.agreement.TestingDtoMapperUtil.checkDtoCreatingException;
import static com.academy.fintech.util.TestingRequestUtil.genRandomAgreementId;

public class DisbursementDtoMapperTest {

    @Test
    public void testSyntaxIncorrectAgreementId() {
        testDtoException(createRandomGoodActivateEndPointRequest("abracadabra"));
        testDtoException(createRandomGoodActivateEndPointRequest("123456789"));
    }

    @Test
    public void testSyntaxIncorrectDate() {
        testDtoException(createRandomGoodActivateEndPointRequest(genRandomAgreementId())
                .setData("2020.11.12")
        );
        testDtoException(createRandomGoodActivateEndPointRequest(genRandomAgreementId())
                .setData("abracadabra")
        );
    }

    @Test
    public void testGoodRequest() {
        for (int i = 0; i < 100; i++) {
            Assertions.assertDoesNotThrow(() ->
                    DtoMapper.createDisbursementDto(
                            createRandomGoodActivateEndPointRequest(genRandomAgreementId()).build()
                    )
            );
        }
    }

    private void testDtoException(DisbursementRequest.Builder disbursementBuilder) {
        checkDtoCreatingException(() -> DtoMapper.createDisbursementDto(disbursementBuilder.build()));
    }
}
