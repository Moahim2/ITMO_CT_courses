package com.academy.fintech.origination.grpc.integrate_test.end_point.create;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import com.academy.fintech.origination.grpc.integrate_test.db.application.ApplicationDataBaseTest;
import com.academy.fintech.origination.grpc.integrate_test.db.application.entity.TestApplicationEntity;
import com.academy.fintech.origination.grpc.integrate_test.db.client.ClientDataBaseTest;
import com.academy.fintech.origination.grpc.integrate_test.db.client.entity.TestClientEntity;
import com.academy.fintech.origination.grpc.integrate_test.end_point.RequestIT;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCreateRequestAndGetId;
import static com.academy.fintech.util.TestingRequestUtil.PREFIX_CLIENT_ID;
import static com.academy.fintech.util.TestingRequestUtil.checkCorrectUUIDWithPrefix;

public interface CreateEndPointDataBaseIT extends RequestIT, ApplicationDataBaseTest, ClientDataBaseTest {

    @Test
    default void testCreateWithRandomGoodRequestAndTestDBDate() {
        for (int i = 0; i < 10; i++) {
            ApplicationRequest request = createRandomGoodCreateEndPointRequest().build();

            String applicationId = sendCreateRequestAndGetId(request, getStub());

            TestApplicationEntity application = getTestApplicationRepository().findById(applicationId).orElseThrow();

            TestClientEntity client = getTestClientRepository().findById(application.clientId).orElseThrow();

            checkDBApplicationFields(request, client, application);
        }
    }


    private static void checkDBApplicationFields(ApplicationRequest request,
                                                 TestClientEntity client,
                                                 TestApplicationEntity application) {
        checkCorrectUUIDWithPrefix(PREFIX_CLIENT_ID, application.clientId);

        Assertions.assertEquals(request.getFirstName(), client.firstName);
        Assertions.assertEquals(request.getLastName(), client.lastName);
        Assertions.assertEquals(request.getEmail(), client.email);
        Assertions.assertEquals(request.getSalary(), client.salary);

        Assertions.assertEquals(ApplicationStatus.NEW, application.status);
        Assertions.assertEquals(request.getDisbursementAmount(), application.disbursementAmount);
    }
}
