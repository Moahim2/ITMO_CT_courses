package com.academy.fintech.api.public_interface.application.dto;

import lombok.Builder;

@Builder
public record ApplicationDto (
        String firstName,
        String lastName,
        String email,
        int salary,
        int amount
) { }
