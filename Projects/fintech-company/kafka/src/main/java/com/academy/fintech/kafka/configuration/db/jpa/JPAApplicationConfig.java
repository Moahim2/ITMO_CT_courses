package com.academy.fintech.kafka.configuration.db.jpa;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.ApplicationRepository;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.entity.ApplicationEntity;
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
        entityManagerFactoryRef = "applicationEntityManagerFactory",
        transactionManagerRef = "applicationTransactionManager",
        basePackageClasses = { ApplicationRepository.class }
)
public class JPAApplicationConfig {

    @Bean(name="applicationEntityManagerFactory")
    public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
            (EntityManagerFactoryBuilder builder,
             @Qualifier("applicationDatasource") DataSource dataSource){

        return builder.dataSource(dataSource)
                .packages(ApplicationEntity.class)
                .build();
    }

    @Bean(name = "applicationTransactionManager")
    public PlatformTransactionManager transactionManager(
            @Qualifier("applicationEntityManagerFactory") EntityManagerFactory entityManagerFactory) {

        return new JpaTransactionManager(entityManagerFactory);
    }
}

