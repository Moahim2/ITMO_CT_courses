package com.academy.fintech.pe.grpc.integrate_test;

import com.academy.fintech.agreement.AgreementServiceGrpc;
import com.academy.fintech.pe.grpc.integrate_test.db.payment_schedule_payment.TestPaymentSchedulePaymentRepository;
import com.academy.fintech.pe.grpc.integrate_test.end_point.activate.ActivateEndPointDataBaseIT;
import com.academy.fintech.pe.grpc.integrate_test.end_point.create.CreateEndPointResponseIT;
import com.academy.fintech.pe.grpc.integrate_test.db.agreement.TestAgreementRepository;
import com.academy.fintech.pe.grpc.integrate_test.end_point.activate.ActivateEndPointResponseIT;
import com.academy.fintech.pe.grpc.integrate_test.end_point.create.CreateEndPointDataBaseIT;
import com.academy.fintech.pe.grpc.integrate_test.environment.black_box.BlackBoxContainers;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static com.academy.fintech.pe.grpc.integrate_test.environment.black_box.BlackBoxContainers.APPLICATION_CONTAINER;

/**
 * Test all end-points (with all tests) on two independent and connected containers over a common network.
 */
@ExtendWith(BlackBoxContainers.class)
@SpringBootTest(properties = {
        "spring.liquibase.enabled=false"
})
public class BlackBoxEnvironmentIT implements
        CreateEndPointResponseIT, CreateEndPointDataBaseIT, ActivateEndPointResponseIT, ActivateEndPointDataBaseIT {
    private ManagedChannel channel;
    private AgreementServiceGrpc.AgreementServiceBlockingStub stub;

    @Autowired
    private TestAgreementRepository testAgreementRepository;

    @Autowired
    private TestPaymentSchedulePaymentRepository testPaymentSchedulePaymentRepository;

    @BeforeEach
    public void init() {
        channel = ManagedChannelBuilder.forAddress(
                "localhost", APPLICATION_CONTAINER.getGrpcPort()
        ).usePlaintext().build();
        stub = AgreementServiceGrpc.newBlockingStub(channel);
    }

    @AfterEach
    public void close() {
        channel.shutdownNow();
    }

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
