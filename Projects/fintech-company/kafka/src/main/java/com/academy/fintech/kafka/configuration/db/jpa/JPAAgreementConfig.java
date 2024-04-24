package com.academy.fintech.kafka.configuration.db.jpa;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.AgreementRepository;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity.AgreementEntity;
import jakarta.persistence.EntityManagerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        entityManagerFactoryRef = "agreementEntityManagerFactory",
        transactionManagerRef = "agreementTransactionManager",
        basePackageClasses = { AgreementRepository.class }
)
public class JPAAgreementConfig {

    @Bean(name="agreementEntityManagerFactory")
    public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
            (EntityManagerFactoryBuilder builder,
             @Qualifier("agreementDatasource") DataSource dataSource){

        return builder.dataSource(dataSource)
                .packages(AgreementEntity.class)
                .build();
    }

    @Bean(name = "agreementTransactionManager")
    public PlatformTransactionManager transactionManager(
            @Qualifier("agreementEntityManagerFactory") EntityManagerFactory entityManagerFactory) {

        return new JpaTransactionManager(entityManagerFactory);
    }
}
