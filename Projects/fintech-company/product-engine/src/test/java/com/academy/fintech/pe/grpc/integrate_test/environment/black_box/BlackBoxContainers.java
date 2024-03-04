package com.academy.fintech.pe.grpc.integrate_test.environment.black_box;

import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.TestInstancePostProcessor;
import org.junit.jupiter.api.extension.TestInstancePreDestroyCallback;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.images.builder.ImageFromDockerfile;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.Future;

@SpringBootTest
public class BlackBoxContainers implements TestInstancePostProcessor, TestInstancePreDestroyCallback {
    private static final Network NETWORK = Network.newNetwork();
    private static final int DB_PORT = 5432;

    public static final PostgreSQLContainer<?> DB_CONTAINER = new PostgreSQLContainer<>("postgres:14.1-alpine")
            .withNetwork(NETWORK)
            .withNetworkAliases("postgresql")
            .withDatabaseName("fintech_pe")
            .withUsername("postgres_pe")
            .withPassword("postgres_pe")
            .withExposedPorts(DB_PORT)
            .withLogConsumer(new Slf4jLogConsumer(LoggerFactory.getLogger(PostgreSQLContainer.class)));

    static {
        String port = Integer.toString(DB_PORT);
        DB_CONTAINER.setPortBindings(List.of(port + ":" + port));
    }

    @Override
    public void postProcessTestInstance(Object o, ExtensionContext extensionContext) {
        APPLICATION_CONTAINER.start();
    }

    @Override
    public void preDestroyTestInstance(ExtensionContext extensionContext) {
        APPLICATION_CONTAINER.close();
    }

    public static class ApplicationContainer extends GenericContainer<ApplicationContainer> {
        private static final int GRPC_PORT = 9091;

        public ApplicationContainer() {
            super(image());
        }

        public int getGrpcPort() {
            return this.getMappedPort(GRPC_PORT);
        }

        @Override
        protected void configure() {
            super.configure();
            withExposedPorts(GRPC_PORT);
            withStartupTimeout(Duration.ofMinutes(5));
        }

        private static Future<String> image() {
            Path dockerfilePath = Paths.get(System.getProperty("user.dir"), "Dockerfile");
            return new ImageFromDockerfile("black-box-test-app", true).withDockerfile(dockerfilePath);
        }
    }

    public static final ApplicationContainer APPLICATION_CONTAINER = new ApplicationContainer()
            .withNetwork(NETWORK)
            .dependsOn(DB_CONTAINER)
            .withNetworkAliases("app")
//            .withEnv("DB_HOST", "postgresql")
            .withEnv("DB_NAME", DB_CONTAINER.getDatabaseName())
            .withEnv("DB_USERNAME", DB_CONTAINER.getUsername())
            .withEnv("DB_PASSWORD", DB_CONTAINER.getPassword())
            .withLogConsumer(new Slf4jLogConsumer(LoggerFactory.getLogger(ApplicationContainer.class)));
}
