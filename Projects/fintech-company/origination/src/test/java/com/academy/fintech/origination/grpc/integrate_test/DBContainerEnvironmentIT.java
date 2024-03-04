package com.academy.fintech.origination.grpc.integrate_test;

import com.academy.fintech.application.ApplicationServiceGrpc;
import com.academy.fintech.common.grpc.integrate_test.environment.db_container.ContainerDBInitializer;
import com.academy.fintech.origination.grpc.integrate_test.db.application.TestApplicationRepository;
import com.academy.fintech.origination.grpc.integrate_test.db.client.TestClientRepository;
import com.academy.fintech.origination.grpc.integrate_test.end_point.cancel.CancelEndPointDataBaseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.cancel.CancelEndPointResponseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.create.CreateEndPointDataBaseIT;
import com.academy.fintech.origination.grpc.integrate_test.end_point.create.CreateEndPointResponseIT;
import net.devh.boot.grpc.client.inject.GrpcClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Test all end-points (with all tests) on the current device with the database connected.
 */
@SpringBootTest(properties = {
        "spring.scheduler.enabled=false"
})
@ContextConfiguration(initializers = {ContainerDBInitializer.class})
@Testcontainers
public class DBContainerEnvironmentIT implements
        CreateEndPointResponseIT,
        CreateEndPointDataBaseIT,
        CancelEndPointResponseIT,
        CancelEndPointDataBaseIT {
    @GrpcClient("inProcess")
    public ApplicationServiceGrpc.ApplicationServiceBlockingStub stub;

    @Autowired
    private TestApplicationRepository testApplicationRepository;

    @Autowired
    private TestClientRepository testClientRepository;

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
