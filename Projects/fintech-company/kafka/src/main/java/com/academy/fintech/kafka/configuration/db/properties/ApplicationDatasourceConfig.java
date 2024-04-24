package com.academy.fintech.kafka.configuration.db.properties;

import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ApplicationDatasourceConfig {

    @Bean
    @ConfigurationProperties("spring.datasource.application")
    public DataSourceProperties applicationDataSourceProperties() {
        return new DataSourceProperties();
    }

}
