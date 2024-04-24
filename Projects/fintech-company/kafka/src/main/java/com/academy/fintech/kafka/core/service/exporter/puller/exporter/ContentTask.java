package com.academy.fintech.kafka.core.service.exporter.puller.exporter;

import lombok.Builder;

@Builder
public record ContentTask<T>(
        long taskId,
        T content
) {}
