package com.academy.agreement.db.agreement;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;

@Service
@RequiredArgsConstructor
public class AgreementService {

    private final AgreementRepository agreementRepository;

    public Agreement insert(String status) {
        return agreementRepository.insert(status);
    }

    public Flux<Agreement> findByStatus(String status) {
        return agreementRepository.findByStatus(status);
    }

}
