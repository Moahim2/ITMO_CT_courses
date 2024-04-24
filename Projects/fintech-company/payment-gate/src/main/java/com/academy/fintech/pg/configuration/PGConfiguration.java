package com.academy.fintech.pg.configuration;

import com.academy.fintech.pg.core.mp.client.rest.MPRestClientProperty;
import com.academy.fintech.pg.core.origination.client.grpc.OriginationGrpcClientProperty;
import com.academy.fintech.pg.core.pe.client.grpc.PEGrpcClientProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableConfigurationProperties({
        MPRestClientProperty.class,
        OriginationGrpcClientProperty.class,
        PEGrpcClientProperty.class
})
public class PGConfiguration {}
