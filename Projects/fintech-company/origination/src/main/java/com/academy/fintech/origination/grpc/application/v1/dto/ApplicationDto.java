package com.academy.fintech.origination.grpc.application.v1.dto;

import com.academy.fintech.application.ApplicationRequest;
import lombok.Builder;

@Builder
public record ApplicationDto(
        String firstName,
        String lastName,
        String email,
        int salary,
        int disbursementAmount
) {

    public static ApplicationDto fromRequest(ApplicationRequest request) {
        return ApplicationDto.builder()
                .firstName(request.getFirstName())
                .lastName(request.getLastName())
                .email(request.getEmail())
                .salary(request.getSalary())
                .disbursementAmount(request.getDisbursementAmount())
                .build();
    }
}
