package com.academy.fintech.common.grpc.integrate_test.environment.db_container;

import org.springframework.boot.test.util.TestPropertyValues;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;

public final class ContainerDBInitializer implements ApplicationContextInitializer<ConfigurableApplicationContext> {
    @Container
    public static final PostgreSQLContainer<?> postgreSQLContainer =
            new PostgreSQLContainer<>("postgres:14.1-alpine")
                    .withDatabaseName("test-DB")
                    .withUsername("test-login")
                    .withPassword("test-password");

    public void initialize(ConfigurableApplicationContext configurableApplicationContext) {
        postgreSQLContainer.start();
        TestPropertyValues.of(
                "spring.datasource.url=" + postgreSQLContainer.getJdbcUrl(),
                "spring.datasource.username=" + postgreSQLContainer.getUsername(),
                "spring.datasource.password=" + postgreSQLContainer.getPassword(),
                "spring.liquibase.enabled=true",
                "grpc.server.inProcessName=test",
                "grpc.server.port=-1",
                "grpc.client.inProcess.address=in-process:test"
        ).applyTo(configurableApplicationContext.getEnvironment());
    }
}
