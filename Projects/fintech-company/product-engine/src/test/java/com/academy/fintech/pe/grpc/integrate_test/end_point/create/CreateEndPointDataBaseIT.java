package com.academy.fintech.pe.grpc.integrate_test.end_point.create;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.AgreementDataBaseTest;
import com.academy.fintech.pe.grpc.integrate_test.end_point.RequestIT;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.TestAgreementEntity;
import com.academy.fintech.pe.grpc.TestingAgreementRequestUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

import static com.academy.fintech.pe.TestngFinanceUtil.assertEqualsBigDecimal;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodCreateEndPointRequest;

/**
 * Tests for checking real values in table of agreements in DataBase.
 */
public interface CreateEndPointDataBaseIT extends RequestIT, AgreementDataBaseTest {

    @Transactional
    @Test
    default void testCreateWithRandomGoodRequestAndTestDBDate() {
        for (int i = 0; i < 100; i++) {
            AgreementRequest request = createRandomGoodCreateEndPointRequest().build();

            AgreementResponse response = getStub().create(request);
            Assertions.assertEquals(0, response.getResponseCode());
            Assertions.assertEquals("", response.getErrorMessage());

            Optional<TestAgreementEntity> agreementOpt =
                    getTestAgreementRepository().findById(response.getAgreementId());
            Assertions.assertTrue(agreementOpt.isPresent());

            TestAgreementEntity agreement = agreementOpt.get();

            checkDBAgreementFields(request, agreement);
        }
    }

    private static void checkDBAgreementFields(AgreementRequest request, TestAgreementEntity agreement) {
        Assertions.assertEquals(request.getClientId(), agreement.clientId);
        Assertions.assertEquals(request.getLoanTerm(), agreement.loanTermInMonths);
        assertEqualsBigDecimal(
                TestingAgreementRequestUtil.convertToBigDecimal(request.getDisbursementAmount())
                        .add(TestingAgreementRequestUtil.convertToBigDecimal(request.getOriginationAmount())),
                agreement.principalAmount);
        TestingAgreementRequestUtil.assertEqualsBigDecimalValue(request.getOriginationAmount(), agreement.originationAmount);
        TestingAgreementRequestUtil.assertEqualsBigDecimalValue(request.getInterest(), agreement.interest);
        Assertions.assertEquals("CL1.0", agreement.productCode);
        Assertions.assertEquals(AgreementStatus.WAITING, agreement.status);
    }
}
