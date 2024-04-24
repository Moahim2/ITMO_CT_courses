package com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;

public record ExportTaskTypeInfo(
        ExportType exportType,
        long countToProcess
) {}
