package com.academy.fintech.kafka;

import com.academy.fintech.kafka.core.service.exporter.puller.dispatcher.property.ExportTaskRetryStalledProperty;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.gson.GsonAutoConfiguration;
import org.springframework.boot.autoconfigure.http.HttpMessageConvertersAutoConfiguration;
import org.springframework.boot.autoconfigure.netty.NettyAutoConfiguration;
import org.springframework.boot.autoconfigure.web.client.RestTemplateAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.HttpEncodingAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.MultipartAutoConfiguration;
import org.springframework.boot.autoconfigure.web.servlet.error.ErrorMvcAutoConfiguration;
import org.springframework.boot.autoconfigure.websocket.servlet.WebSocketServletAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.FullyQualifiedAnnotationBeanNameGenerator;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableScheduling
@EnableConfigurationProperties({ ExportTaskRetryStalledProperty.class })
@SpringBootApplication(
        exclude = {
                GsonAutoConfiguration.class,
                MultipartAutoConfiguration.class,
                WebSocketServletAutoConfiguration.class,
                NettyAutoConfiguration.class,
                RestTemplateAutoConfiguration.class,
                HttpMessageConvertersAutoConfiguration.class,
                HttpEncodingAutoConfiguration.class,
                ErrorMvcAutoConfiguration.class
        }
)
public class Application {

    public static void main(String[] args) {
        new SpringApplicationBuilder(Application.class)
                .beanNameGenerator(new FullyQualifiedAnnotationBeanNameGenerator())
                .run(args);
    }

}
