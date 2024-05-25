package com.academy.agreement.db.agreement;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface AgreementRepository extends ReactiveCrudRepository<Agreement, Long> {

    default Agreement insert(String status) {
        return save(Agreement.builder().status(status).build()).block();
    }

    Flux<Agreement> findByStatus(String status);
}
