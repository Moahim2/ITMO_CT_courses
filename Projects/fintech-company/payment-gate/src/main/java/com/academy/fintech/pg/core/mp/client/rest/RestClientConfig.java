package com.academy.fintech.pg.core.mp.client.rest;

import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;

@Configuration
public class RestClientConfig {

    @Bean
    public RestTemplate restTemplate(MPRestClientProperty property) {
        return new RestTemplateBuilder()
                .uriTemplateHandler(new DefaultUriBuilderFactory(formAddress(property.host(), property.port())))
                .build();
    }

    private static String formAddress(String host, int port) {
        return "http://%s:%d/".formatted(host, port);
    }
}
