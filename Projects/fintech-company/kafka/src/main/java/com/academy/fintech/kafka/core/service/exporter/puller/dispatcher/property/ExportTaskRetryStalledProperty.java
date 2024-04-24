package com.academy.fintech.kafka.core.service.exporter.puller.dispatcher.property;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "exporter.dispatcher.retry-stalled")
public record ExportTaskRetryStalledProperty (int passedMinutesInProcessingToRetry) {}
