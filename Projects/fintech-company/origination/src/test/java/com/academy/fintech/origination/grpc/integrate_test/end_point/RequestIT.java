package com.academy.fintech.origination.grpc.integrate_test.end_point;

import com.academy.fintech.application.ApplicationServiceGrpc;

public interface RequestIT {
    ApplicationServiceGrpc.ApplicationServiceBlockingStub getStub();
}
