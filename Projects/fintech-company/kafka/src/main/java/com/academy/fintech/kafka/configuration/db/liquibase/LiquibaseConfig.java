package com.academy.fintech.kafka.configuration.db.liquibase;

import org.springframework.boot.autoconfigure.liquibase.LiquibaseProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class LiquibaseConfig {

    @Bean
    @ConfigurationProperties(prefix = "spring.datasource.exporter.liquibase")
    public LiquibaseProperties exporterLiquibaseProperties() {
        return new LiquibaseProperties();
    }

}
