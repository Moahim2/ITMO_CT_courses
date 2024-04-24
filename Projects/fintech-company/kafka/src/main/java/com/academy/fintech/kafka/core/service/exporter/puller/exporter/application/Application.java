package com.academy.fintech.kafka.core.service.exporter.puller.exporter.application;

import lombok.Builder;

@Builder
public record Application(
        String id,
        String clientId,
        int disbursementAmount,
        String status,
        String createdAt,
        String businessDate
) {}
