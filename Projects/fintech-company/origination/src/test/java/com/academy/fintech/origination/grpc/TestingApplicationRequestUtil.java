package com.academy.fintech.origination.grpc;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.application.ApplicationResponse;
import com.academy.fintech.application.ApplicationServiceGrpc;
import com.academy.fintech.application.CancelRequest;
import com.academy.fintech.application.CancelResponse;
import io.grpc.StatusRuntimeException;
import lombok.experimental.UtilityClass;

import static com.academy.fintech.util.RandomUtil.genRandomInt;
import static com.academy.fintech.util.TestingRequestUtil.PREFIX_APPLICATION_ID;
import static com.academy.fintech.util.TestingRequestUtil.checkCorrectUUIDWithPrefix;
import static com.academy.fintech.util.TestingRequestUtil.genRandomEngName;
import static com.academy.fintech.util.TestingRequestUtil.genRandomNewEmail;

@UtilityClass
public class TestingApplicationRequestUtil {

    public static String sendCreateRequestAndGetId(ApplicationRequest request,
                                                   ApplicationServiceGrpc.ApplicationServiceBlockingStub stub)
    throws StatusRuntimeException {
        ApplicationResponse response = stub.create(request);
        checkCorrectUUIDWithPrefix(PREFIX_APPLICATION_ID, response.getApplicationId());
        return response.getApplicationId();
    }

    public static boolean sendCancelRequestAndGetResult(CancelRequest request,
                                                        ApplicationServiceGrpc.ApplicationServiceBlockingStub stub)
    throws StatusRuntimeException {
        CancelResponse response = stub.cancel(request);
        return response.getIsCanceled();
    }

    public static ApplicationRequest.Builder createRandomGoodCreateEndPointRequest() {
        return ApplicationRequest.newBuilder()
                .setFirstName(genRandomEngName())
                .setLastName(genRandomEngName())
                .setEmail(genRandomNewEmail())
                .setSalary(genRandomInt(0, Integer.MAX_VALUE))
                .setDisbursementAmount(genRandomInt(0, Integer.MAX_VALUE));
    }

    public static CancelRequest.Builder createRandomGoodCancelEndPointRequest(String applicationId) {
        return CancelRequest.newBuilder()
                .setApplicationId(applicationId);
    }
}
