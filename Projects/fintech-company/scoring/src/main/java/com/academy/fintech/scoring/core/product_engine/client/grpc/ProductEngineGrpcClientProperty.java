package com.academy.fintech.scoring.core.product_engine.client.grpc;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "scoring.client.product-engine.grpc")
public record ProductEngineGrpcClientProperty(String host, int port) {}
