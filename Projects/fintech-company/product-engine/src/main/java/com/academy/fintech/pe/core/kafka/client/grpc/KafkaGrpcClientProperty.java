package com.academy.fintech.pe.core.kafka.client.grpc;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "pe.client.kafka.grpc")
public record KafkaGrpcClientProperty(String host, int port) {}
