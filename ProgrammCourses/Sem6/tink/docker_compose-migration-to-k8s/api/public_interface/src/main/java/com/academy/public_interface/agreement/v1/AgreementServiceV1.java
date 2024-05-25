package com.academy.public_interface.agreement.v1;

import reactor.core.publisher.Flux;

import java.util.List;

public interface AgreementServiceV1 {

    AgreementResponseDto create(String status);

    Flux<AgreementResponseDto> get(String status);

}
