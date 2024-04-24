package com.academy.fintech.pg.core.mp.client.rest;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "pg.client.mp.rest")
public record MPRestClientProperty(String host, int port) { }
