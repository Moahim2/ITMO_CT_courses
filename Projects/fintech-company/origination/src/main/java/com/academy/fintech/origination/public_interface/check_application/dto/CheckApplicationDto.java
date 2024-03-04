package com.academy.fintech.origination.public_interface.check_application.dto;

import lombok.Builder;

@Builder
public record CheckApplicationDto(String applicationId, String clientId, int salary, int disbursementAmount) {}

