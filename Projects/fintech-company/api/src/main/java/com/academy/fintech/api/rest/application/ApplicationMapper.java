package com.academy.fintech.api.rest.application;

import com.academy.fintech.api.public_interface.application.dto.ApplicationDto;
import org.springframework.stereotype.Component;

@Component
public class ApplicationMapper {

    public ApplicationDto mapRequestToDto(ApplicationRequest request) {
        return ApplicationDto.builder()
                .firstName(request.firstName())
                .lastName(request.lastName())
                .email(request.email())
                .amount(request.amount())
                .salary(request.salary())
                .build();
    }

}
