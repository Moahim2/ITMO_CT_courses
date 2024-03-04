package com.academy.fintech.api.core.origination.client.grpc;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "api.client.origination.grpc")
public record OriginationGrpcClientProperty(String host, int port) { }
