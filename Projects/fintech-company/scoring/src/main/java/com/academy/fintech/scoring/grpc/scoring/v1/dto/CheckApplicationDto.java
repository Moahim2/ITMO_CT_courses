package com.academy.fintech.scoring.grpc.scoring.v1.dto;

import lombok.Builder;

@Builder
public record CheckApplicationDto(String clientId, int salary, int disbursementAmount) {}
