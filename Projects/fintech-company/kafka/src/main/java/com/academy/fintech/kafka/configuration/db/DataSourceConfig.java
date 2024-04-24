package com.academy.fintech.kafka.configuration.db;

import com.academy.fintech.kafka.configuration.db.liquibase.LiquibaseConfig;
import com.academy.fintech.kafka.configuration.db.properties.AgreementDatasourceConfig;
import com.academy.fintech.kafka.configuration.db.properties.ApplicationDatasourceConfig;
import com.academy.fintech.kafka.configuration.db.properties.ExporterDatasourceConfig;
import liquibase.integration.spring.SpringLiquibase;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.liquibase.LiquibaseProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import javax.sql.DataSource;

@Configuration
@RequiredArgsConstructor
public class DataSourceConfig {
    private final AgreementDatasourceConfig agreementDatasourceConfig;
    private final ApplicationDatasourceConfig applicationDatasourceConfig;
    private final ExporterDatasourceConfig exporterDatasourceConfig;
    private final LiquibaseConfig liquibaseConfig;

    @Primary
    @Bean(name = "exportDatasource")
    @ConfigurationProperties("spring.datasource.exporter.hikari")
    public DataSource exporterDataSource() {
        return exporterDatasourceConfig.exporterDataSourceProperties()
                .initializeDataSourceBuilder()
                .build();
    }

    @Bean
    public SpringLiquibase exporterLiquibase() {
        return springLiquibase(exporterDataSource(), liquibaseConfig.exporterLiquibaseProperties());
    }

    @Bean(name = "applicationDatasource")
    public DataSource applicationDataSource() {
        return applicationDatasourceConfig.applicationDataSourceProperties()
                .initializeDataSourceBuilder()
                .build();
    }

    @Bean(name = "agreementDatasource")
    public DataSource agreementDataSource() {
        return agreementDatasourceConfig.agreementDataSourceProperties()
                .initializeDataSourceBuilder()
                .build();
    }

    private static SpringLiquibase springLiquibase(DataSource dataSource, LiquibaseProperties properties) {
        SpringLiquibase liquibase = new SpringLiquibase();
        liquibase.setDataSource(dataSource);
        liquibase.setChangeLog(properties.getChangeLog());
        liquibase.setContexts(properties.getContexts());
        liquibase.setDefaultSchema(properties.getDefaultSchema());
        liquibase.setDropFirst(properties.isDropFirst());
        liquibase.setShouldRun(properties.isEnabled());
        liquibase.setLabelFilter(properties.getLabelFilter());
        liquibase.setChangeLogParameters(properties.getParameters());
        liquibase.setRollbackFile(properties.getRollbackFile());
        return liquibase;
    }

}
