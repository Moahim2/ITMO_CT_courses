package com.academy.fintech.dwh.core.service.export.application;

import lombok.Builder;

@Builder
public record Application(
        String id,
        String clientId,
        int disbursementAmount,
        String status,
        String createdAt,
        String updatedAt,
        String businessDate
) {}
