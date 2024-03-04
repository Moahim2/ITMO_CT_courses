package com.academy.fintech.pe.grpc.integrate_test.end_point;

import com.academy.fintech.agreement.AgreementServiceGrpc;

public interface RequestIT {
    AgreementServiceGrpc.AgreementServiceBlockingStub getStub();
}
