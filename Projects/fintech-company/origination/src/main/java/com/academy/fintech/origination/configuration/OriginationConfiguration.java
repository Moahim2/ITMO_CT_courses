package com.academy.fintech.origination.configuration;

import com.academy.fintech.origination.core.kafka.client.grpc.KafkaGrpcClientProperty;
import com.academy.fintech.origination.core.pe.client.grpc.PEGrpcClientProperty;
import com.academy.fintech.origination.core.pg.client.grpc.PGGrpcClientProperty;
import com.academy.fintech.origination.core.scoring.client.grpc.ScoringGrpcClientProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties({
        ScoringGrpcClientProperty.class,
        PGGrpcClientProperty.class,
        PEGrpcClientProperty.class,
        KafkaGrpcClientProperty.class
})
public class OriginationConfiguration {}
