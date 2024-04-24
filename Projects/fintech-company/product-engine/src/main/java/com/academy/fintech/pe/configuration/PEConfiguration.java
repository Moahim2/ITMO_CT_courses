package com.academy.fintech.pe.configuration;

import com.academy.fintech.pe.core.kafka.client.grpc.KafkaGrpcClientProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties({
        KafkaGrpcClientProperty.class
})
public class PEConfiguration {}
