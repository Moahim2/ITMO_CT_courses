package com.academy.fintech.pe.grpc.integrate_test.end_point.activate;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentStatus;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.AgreementDataBaseTest;
import com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment.PaymentScheduleDataBaseTest;
import com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment.TestPaymentSchedulePaymentEntity;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.TestAgreementEntity;
import com.academy.fintech.pe.grpc.integrate_test.end_point.RequestIT;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import static com.academy.fintech.pe.TestngFinanceUtil.compareIpmt;
import static com.academy.fintech.pe.TestngFinanceUtil.comparePmt;
import static com.academy.fintech.pe.TestngFinanceUtil.comparePpmt;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodActivateEndPointRequest;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.pe.grpc.TestingAgreementRequestUtil.sendActivateRequestAndGetId;

/**
 * Tests for checking real values in table of payments in DataBase.
 */
public interface ActivateEndPointDataBaseIT extends RequestIT, AgreementDataBaseTest, PaymentScheduleDataBaseTest {

    @Test
    default void testActivateWithRandomGoodRequestAndTestDBDate() {
        for (int i = 0; i < 100; i++) {
            AgreementRequest agreementRequest = createRandomGoodCreateEndPointRequest().build();

            AgreementResponse createResponse = getStub().create(agreementRequest);

            Optional<TestAgreementEntity> agreementOpt =
                    getTestAgreementRepository().findById(createResponse.getAgreementId());
            Assertions.assertTrue(agreementOpt.isPresent());
            TestAgreementEntity agreement = agreementOpt.get();
            DisbursementRequest disbursementRequest = createRandomGoodActivateEndPointRequest(agreement.id).build();

            String paymentScheduleId = sendActivateRequestAndGetId(disbursementRequest, getStub());

            getTestPaymentScheduleRepository().flush();

            Optional<TestAgreementEntity> updatedAgreementOpt =
                    getTestAgreementRepository().findById(createResponse.getAgreementId());
            Assertions.assertTrue(updatedAgreementOpt.isPresent());

            TestAgreementEntity updatedAgreement =  updatedAgreementOpt.get();
            Assertions.assertEquals(AgreementStatus.ACTIVE, updatedAgreement.status);

            List<TestPaymentSchedulePaymentEntity> sortedPayments = getTestPaymentScheduleRepository()
                    .findAllByPaymentScheduleId(paymentScheduleId)
                    .stream()
                    .sorted(Comparator.comparing(TestPaymentSchedulePaymentEntity::getPaymentScheduleId))
                    .toList();

            checkAllPaymentsCalculatedValues(updatedAgreement, sortedPayments);
        }
    }

    private static void checkAllPaymentsCalculatedValues(TestAgreementEntity agreement,
                                                         List<TestPaymentSchedulePaymentEntity> sortedPayments) {
        BigDecimal r = agreement.interest.divide(BigDecimal.valueOf(100), 5, RoundingMode.CEILING);
        int nper = agreement.loanTermInMonths;
        BigDecimal pv = agreement.principalAmount;

        LocalDate lastDate = agreement.disbursementDate;
        for (int j = 1; j < sortedPayments.size() + 1; j++) {
            TestPaymentSchedulePaymentEntity payment = sortedPayments.get(j - 1);

            Assertions.assertEquals(j, payment.periodNumber);
            checkDateDistance(lastDate, payment.paymentDate);
            comparePmt(r, nper, pv, payment.periodPayment);
            compareIpmt(r, j, nper, pv, payment.interestPayment);
            comparePpmt(r, j, nper, pv, payment.principalPayment);
            Assertions.assertEquals(PaymentStatus.FUTURE, payment.status);

            lastDate = payment.paymentDate;
        }
    }

    private static void checkDateDistance(LocalDate lastDate, LocalDate curDate) {
        int monthDist = (curDate.getMonthValue() + 12 - lastDate.getMonthValue()) % 12;
        int dayDist = curDate.getDayOfMonth() - lastDate.getDayOfMonth();
        Assertions.assertEquals(1, monthDist);
        Assertions.assertTrue(dayDist <= 4);
        Assertions.assertTrue(dayDist >= -4);
    }
}
