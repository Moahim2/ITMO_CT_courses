package com.academy.fintech.api.configuration;

import com.academy.fintech.api.core.origination.client.grpc.OriginationGrpcClientProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties({ OriginationGrpcClientProperty.class })
public class ApiConfiguration {}
