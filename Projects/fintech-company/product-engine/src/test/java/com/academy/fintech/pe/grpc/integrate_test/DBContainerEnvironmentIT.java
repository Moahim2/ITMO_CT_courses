package com.academy.fintech.pe.grpc.integrate_test;

import com.academy.fintech.agreement.AgreementServiceGrpc;
import com.academy.fintech.common.grpc.integrate_test.environment.db_container.ContainerDBInitializer;
import com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment.TestPaymentSchedulePaymentRepository;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.TestAgreementRepository;
import com.academy.fintech.pe.grpc.integrate_test.end_point.activate.ActivateEndPointDataBaseIT;
import com.academy.fintech.pe.grpc.integrate_test.end_point.activate.ActivateEndPointResponseIT;
import com.academy.fintech.pe.grpc.integrate_test.end_point.create.CreateEndPointDataBaseIT;
import com.academy.fintech.pe.grpc.integrate_test.end_point.create.CreateEndPointResponseIT;
import net.devh.boot.grpc.client.inject.GrpcClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Test all end-points (with all tests) on the current device with the database connected.
 */
@SpringBootTest
@ContextConfiguration(initializers = {ContainerDBInitializer.class})
@Testcontainers
public class DBContainerEnvironmentIT implements
        CreateEndPointResponseIT, CreateEndPointDataBaseIT, ActivateEndPointResponseIT, ActivateEndPointDataBaseIT {
    @GrpcClient("inProcess")
    public AgreementServiceGrpc.AgreementServiceBlockingStub stub;

    @Autowired
    private TestAgreementRepository testAgreementRepository;

    @Autowired
    private TestPaymentSchedulePaymentRepository testPaymentSchedulePaymentRepository;

    @Override
    public AgreementServiceGrpc.AgreementServiceBlockingStub getStub() {
        return stub;
    }

    @Override
    public TestAgreementRepository getTestAgreementRepository() {
        return testAgreementRepository;
    }

    @Override
    public TestPaymentSchedulePaymentRepository getTestPaymentScheduleRepository() {
        return testPaymentSchedulePaymentRepository;
    }
}
