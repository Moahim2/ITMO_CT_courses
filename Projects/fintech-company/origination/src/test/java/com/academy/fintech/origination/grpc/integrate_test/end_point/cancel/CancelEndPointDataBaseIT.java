package com.academy.fintech.origination.grpc.integrate_test.end_point.cancel;

import com.academy.fintech.application.CancelRequest;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import com.academy.fintech.origination.grpc.integrate_test.db.application.ApplicationDataBaseTest;
import com.academy.fintech.origination.grpc.integrate_test.db.application.TestApplicationRepository;
import com.academy.fintech.origination.grpc.integrate_test.db.application.entity.TestApplicationEntity;
import com.academy.fintech.origination.grpc.integrate_test.end_point.RequestIT;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCancelEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCancelRequestAndGetResult;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCreateRequestAndGetId;
import static com.academy.fintech.util.TestingRequestUtil.genRandomApplicationId;

public interface CancelEndPointDataBaseIT extends RequestIT, ApplicationDataBaseTest {

    @Test
    default void testCancelWithRandomGoodRequestAndTestDBDate() {
        List<String> listApplicationId = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            String applicationId = sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
            listApplicationId.add(applicationId);
        }

        for (String id : listApplicationId) {
            CancelRequest request = createRandomGoodCancelEndPointRequest(id).build();

            boolean result = sendCancelRequestAndGetResult(request, getStub());
            Assertions.assertTrue(result);

            TestApplicationEntity application = getTestApplicationRepository().findById(id).orElseThrow();

            checkDBApplicationFields(ApplicationStatus.CLOSED, application);
        }
    }

    @Test
    @Transactional
    default void testCancelWithNotExistingApplicationId() {
        String notExistApplicationId;
        do {
            notExistApplicationId = genRandomApplicationId();
        } while (getTestApplicationRepository().existsById(notExistApplicationId));

        checkResponseNotExistError(
                createRandomGoodCancelEndPointRequest(notExistApplicationId).build()
        );
    }

    @Test
    default void testCancelWithAcceptedApplication() {
        String applicationId = sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
        TestApplicationRepository testApplicationRepository = getTestApplicationRepository();

        TestApplicationEntity application = testApplicationRepository.findById(applicationId).orElseThrow();

        application.status = ApplicationStatus.ACCEPTED;
        testApplicationRepository.save(application);

        checkThatApplicationNotCancelled(applicationId, ApplicationStatus.ACCEPTED);

        application.status = ApplicationStatus.ACTIVE;
        testApplicationRepository.save(application);

        checkThatApplicationNotCancelled(applicationId, ApplicationStatus.ACTIVE);
    }


    private static void checkDBApplicationFields(ApplicationStatus status,
                                                 TestApplicationEntity application) {
        Assertions.assertEquals(status, application.status);
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void checkResponseNotExistError(CancelRequest request) {
        try {
            getStub().cancel(request);
            Assertions.fail("Must be error!");
        } catch (StatusRuntimeException e) {
            Assertions.assertEquals(Status.NOT_FOUND.getCode(), e.getStatus().getCode());
        }
    }

    private void checkThatApplicationNotCancelled(String applicationId, ApplicationStatus curStatus) {
        CancelRequest request = createRandomGoodCancelEndPointRequest(applicationId).build();
        boolean result = sendCancelRequestAndGetResult(request, getStub());
        Assertions.assertFalse(result);
        checkDBApplicationFields(
                curStatus,
                getTestApplicationRepository().findById(applicationId).orElseThrow());
    }
}
