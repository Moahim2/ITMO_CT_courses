package com.academy.fintech.kafka.configuration.db.properties;

import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ExporterDatasourceConfig {

    @Bean
    @ConfigurationProperties("spring.datasource.exporter")
    public DataSourceProperties exporterDataSourceProperties() {
        return new DataSourceProperties();
    }

}
