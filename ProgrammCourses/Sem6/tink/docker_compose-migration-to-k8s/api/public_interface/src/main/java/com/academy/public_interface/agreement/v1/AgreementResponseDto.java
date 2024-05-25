package com.academy.public_interface.agreement.v1;

import lombok.Builder;

@Builder
public record AgreementResponseDto(long id, String status) { }
