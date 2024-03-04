package com.academy.fintech.pe.grpc;

import com.academy.fintech.BigDecimalValue;
import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.AgreementServiceGrpc;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.agreement.DisbursementResponse;
import com.google.protobuf.ByteString;
import org.junit.jupiter.api.Assertions;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import static com.academy.fintech.pe.TestngFinanceUtil.assertEqualsBigDecimal;
import static com.academy.fintech.util.RandomUtil.genRandomDate;
import static com.academy.fintech.util.RandomUtil.genRandomDoubleValue;
import static com.academy.fintech.util.RandomUtil.genRandomInt;
import static com.academy.fintech.util.TestingRequestUtil.PREFIX_AGREEMENT_ID;
import static com.academy.fintech.util.TestingRequestUtil.PREFIX_PAYMENT_SCHEDULE_ID;
import static com.academy.fintech.util.TestingRequestUtil.checkCorrectUUIDWithPrefix;
import static com.academy.fintech.util.TestingRequestUtil.genRandomClientId;

public final class TestingAgreementRequestUtil {

    public static String sendCreateRequestAndGetId(AgreementRequest request,
                                                   AgreementServiceGrpc.AgreementServiceBlockingStub stub) {
        AgreementResponse response = stub.create(request);
        Assertions.assertEquals(0, response.getResponseCode());
        Assertions.assertEquals("", response.getErrorMessage());
        checkCorrectUUIDWithPrefix(PREFIX_AGREEMENT_ID, response.getAgreementId());
        return response.getAgreementId();
    }

    public static String sendActivateRequestAndGetId(DisbursementRequest request,
                                                     AgreementServiceGrpc.AgreementServiceBlockingStub stub) {
        DisbursementResponse response = stub.activate(request);
        Assertions.assertEquals(0, response.getResponseCode());
        Assertions.assertEquals("", response.getErrorMessage());
        checkCorrectUUIDWithPrefix(PREFIX_PAYMENT_SCHEDULE_ID, response.getPaymentScheduleId());
        return response.getPaymentScheduleId();
    }

    public static AgreementRequest.Builder createRandomGoodCreateEndPointRequest() {
        double principalAmount = genRandomDoubleValue(50000, 500000);
        double originationAmount = genRandomDoubleValue(2000, 10000);
        double disbursementAmount = principalAmount - originationAmount;
        return AgreementRequest.newBuilder()
                .setClientId(genRandomClientId())
                .setLoanTerm(genRandomInt(3, 24))
                .setDisbursementAmount(TestingAgreementRequestUtil.toBDValue(Double.toString(disbursementAmount)))
                .setOriginationAmount(TestingAgreementRequestUtil.toBDValue(Double.toString(originationAmount)))
                .setInterest(TestingAgreementRequestUtil.toBDValue(Double.toString(genRandomDoubleValue(8, 15))))
                .setProductName("CL1.0");
    }

    public static DisbursementRequest.Builder createRandomGoodActivateEndPointRequest(String goodAgreementId) {
        return DisbursementRequest.newBuilder()
                .setAgreementId(goodAgreementId)
                .setData(genRandomDate());
    }

    public static BigDecimalValue toBDValue(String value) {
        BigDecimal bigDecimal = new BigDecimal(value);
        return BigDecimalValue.newBuilder()
                .setScale(bigDecimal.scale())
                .setPrecision(bigDecimal.precision())
                .setValue(ByteString.copyFrom(bigDecimal.unscaledValue().toByteArray()))
                .build();
    }

    public static void assertEqualsBigDecimalValue(BigDecimalValue bdValue, BigDecimal val) {
        assertEqualsBigDecimal(convertToBigDecimal(bdValue), val);
    }

    public static BigDecimal convertToBigDecimal(BigDecimalValue decimalValue) {
        return new BigDecimal(
                new BigInteger(decimalValue.getValue().toByteArray()),
                decimalValue.getScale(),
                new MathContext(decimalValue.getPrecision())
        );
    }

    private TestingAgreementRequestUtil() {}
}
