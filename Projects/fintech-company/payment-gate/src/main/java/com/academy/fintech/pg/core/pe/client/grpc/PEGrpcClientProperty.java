package com.academy.fintech.pg.core.pe.client.grpc;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "pg.client.pe.grpc")
public record PEGrpcClientProperty(String host, int port) { }
