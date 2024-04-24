package com.academy.fintech.origination.core.kafka.client.grpc;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "origination.client.kafka.grpc")
public record KafkaGrpcClientProperty(String host, int port) {}
