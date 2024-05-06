package com.academy.fintech.dwh.core.service.export.configuration;

import com.academy.fintech.dwh.core.service.export.agreement.Agreement;
import com.academy.fintech.dwh.core.service.export.application.Application;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.support.serializer.JsonDeserializer;

/**
 * Configuration for auto-deserialization (in listeners) messages from kafka.
 */
@Configuration
public class ConsumerConfiguration {

    @Bean
    public ConsumerFactory<String, Agreement> agreementConsumerFactory(KafkaProperties properties) {
        return new DefaultKafkaConsumerFactory<>(
                properties.buildConsumerProperties(),
                new StringDeserializer(),
                new JsonDeserializer<>(Agreement.class));
    }

    @Bean
    public ConsumerFactory<String, Application> applicationConsumerFactory(KafkaProperties properties) {
        return new DefaultKafkaConsumerFactory<>(
                properties.buildConsumerProperties(),
                new StringDeserializer(),
                new JsonDeserializer<>(Application.class));
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, Agreement> agreementConcurrentKafkaListenerContainerFactory(
            KafkaProperties properties
    ) {
        ConcurrentKafkaListenerContainerFactory<String, Agreement> factory =
                new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(agreementConsumerFactory(properties));
        factory.setBatchListener(true);
        return factory;
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, Application> applicationConcurrentKafkaListenerContainerFactory(
            KafkaProperties properties
    ) {
        ConcurrentKafkaListenerContainerFactory<String, Application> factory =
                new ConcurrentKafkaListenerContainerFactory<>();
        factory.setConsumerFactory(applicationConsumerFactory(properties));
        factory.setBatchListener(true);
        return factory;
    }

}
