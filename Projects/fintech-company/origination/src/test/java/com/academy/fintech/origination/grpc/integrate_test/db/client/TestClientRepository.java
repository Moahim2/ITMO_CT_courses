package com.academy.fintech.origination.grpc.integrate_test.db.client;

import com.academy.fintech.origination.grpc.integrate_test.db.client.entity.TestClientEntity;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

public interface TestClientRepository extends CrudRepository<TestClientEntity, String> {
    Optional<TestClientEntity> findByEmail(String email);
}
