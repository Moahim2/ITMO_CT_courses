package com.academy.fintech.kafka.configuration.db.properties;

import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AgreementDatasourceConfig {

    @Bean
    @ConfigurationProperties("spring.datasource.agreement")
    public DataSourceProperties agreementDataSourceProperties() {
        return new DataSourceProperties();
    }

}
