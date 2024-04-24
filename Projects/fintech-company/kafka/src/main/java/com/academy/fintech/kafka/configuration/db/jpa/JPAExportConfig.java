package com.academy.fintech.kafka.configuration.db.jpa;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.ExportTaskRepository;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.ExportTypeRepository;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTypeEntity;
import jakarta.persistence.EntityManagerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        entityManagerFactoryRef = "exportEntityManagerFactory",
        transactionManagerRef = "exportTransactionManager",
        basePackageClasses = { ExportTaskRepository.class, ExportTypeRepository.class }
)
public class JPAExportConfig {

    @Primary
    @Bean(name="exportEntityManagerFactory")
    public LocalContainerEntityManagerFactoryBean entityManagerFactoryBean
            (EntityManagerFactoryBuilder builder,
             @Qualifier("exportDatasource") DataSource dataSource){

        return builder.dataSource(dataSource)
                .packages(ExportTaskEntity.class, ExportTypeEntity.class)
                .build();
    }

    @Primary
    @Bean(name = "exportTransactionManager")
    public PlatformTransactionManager transactionManager(
            @Qualifier("exportEntityManagerFactory") EntityManagerFactory entityManagerFactory) {

        return new JpaTransactionManager(entityManagerFactory);
    }

}
