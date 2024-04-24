package com.academy.fintech.kafka.grpc.export_task.v1.dto;

import lombok.Builder;

@Builder
public record NewExportTaskDto(
        String type,
        String stringKey,
        Long numberKey
) {}
