package com.academy.fintech.origination.grpc.integrate_test.db.application;

import com.academy.fintech.origination.grpc.integrate_test.db.application.entity.TestApplicationEntity;
import org.springframework.data.repository.CrudRepository;

public interface TestApplicationRepository extends CrudRepository<TestApplicationEntity, String> {}
