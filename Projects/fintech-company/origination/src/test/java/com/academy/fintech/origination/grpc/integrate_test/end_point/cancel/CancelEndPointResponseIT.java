package com.academy.fintech.origination.grpc.integrate_test.end_point.cancel;

import com.academy.fintech.application.CancelRequest;
import com.academy.fintech.origination.grpc.integrate_test.end_point.RequestIT;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCancelEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.createRandomGoodCreateEndPointRequest;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCancelRequestAndGetResult;
import static com.academy.fintech.origination.grpc.TestingApplicationRequestUtil.sendCreateRequestAndGetId;

public interface CancelEndPointResponseIT extends RequestIT {

    @Test
    default void testCancelWithIncorrectSyntaxApplicationId() {
        checkResponseInvalidArgumentError(
                createRandomGoodCancelEndPointRequest("abracadabra").build()
        );

        checkResponseInvalidArgumentError(
                createRandomGoodCancelEndPointRequest("123456789").build()
        );

        checkResponseInvalidArgumentError(
                createRandomGoodCancelEndPointRequest("APP-123123").build()
        );
    }

    @Test
    default void testCancelWithRandomGoodRequest() {
        List<String> listApplicationId = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            String applicationId = sendCreateRequestAndGetId(createRandomGoodCreateEndPointRequest().build(), getStub());
            listApplicationId.add(applicationId);
        }

        for (String id : listApplicationId) {
            Assertions.assertTrue(sendCancelRequestAndGetResult(
                    createRandomGoodCancelEndPointRequest(id).build(),
                    getStub()
            ));
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void checkResponseInvalidArgumentError(CancelRequest request) {
        try {
            getStub().cancel(request);
            Assertions.fail("Must be error!");
        } catch (StatusRuntimeException e) {
            Assertions.assertEquals(Status.INVALID_ARGUMENT.getCode(), e.getStatus().getCode());
        }
    }
}
