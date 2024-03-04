package com.academy.fintech.origination.grpc.integrate_test;

import com.academy.fintech.application.ApplicationServiceGrpc;
import com.academy.fintech.origination.grpc.integrate_test.db.application.TestApplicationRepository;
import com.academy.fintech.origination.grpc.integrate_test.db.client.TestClientRepository;
import com.academy.fintech.origination.grpc.integrate_test.end_point.cancel.CancelEndPointDataBaseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.cancel.CancelEndPointResponseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.create.CreateEndPointDataBaseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.create.CreateEndPointResponseIT;
import com.academy.fintech.origination.grpc.integrate_test.environment.black_box.BlackBoxContainers;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static com.academy.fintech.origination.grpc.integrate_test.environment.black_box.BlackBoxContainers.APPLICATION_CONTAINER;

/**
 * Test all end-points (with all tests) on two independent and connected containers over a common network.
 */
@ExtendWith(BlackBoxContainers.class)
@SpringBootTest(properties = {
        "spring.liquibase.enabled=false",
        "spring.scheduler.enabled=false"
})
public class BlackBoxEnvironmentIT implements
        CreateEndPointResponseIT,
        CreateEndPointDataBaseIT,
        CancelEndPointResponseIT,
        CancelEndPointDataBaseIT {
    private ManagedChannel channel;
    private ApplicationServiceGrpc.ApplicationServiceBlockingStub stub;

    @Autowired
    private TestApplicationRepository testApplicationRepository;

    @Autowired
    private TestClientRepository testClientRepository;

    @BeforeEach
    public void init() {
        channel = ManagedChannelBuilder.forAddress(
                "localhost", APPLICATION_CONTAINER.getGrpcPort()
        ).usePlaintext().build();
        stub = ApplicationServiceGrpc.newBlockingStub(channel);
    }

    @AfterEach
    public void close() {
        channel.shutdownNow();
    }

    @Override
    public ApplicationServiceGrpc.ApplicationServiceBlockingStub getStub() {
        return stub;
    }

    @Override
    public TestApplicationRepository getTestApplicationRepository() {
        return testApplicationRepository;
    }

    @Override
    public TestClientRepository getTestClientRepository() {
        return testClientRepository;
    }
}
