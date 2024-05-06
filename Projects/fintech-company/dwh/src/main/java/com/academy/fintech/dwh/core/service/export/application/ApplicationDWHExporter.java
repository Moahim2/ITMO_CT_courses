package com.academy.fintech.dwh.core.service.export.application;

import com.academy.fintech.dwh.core.service.export.application.db.application_info.ApplicationInfoService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ApplicationDWHExporter {
    private final ApplicationInfoService applicationInfoService;
    private final EntityMapper mapper;

    /**
     * Consumes (with butch) all new application's states from kafka topic and save its to db.
     */
    @KafkaListener(
            topics = "${exporter.application.topic}",
            groupId = "dwh-application",
            containerFactory = "applicationConcurrentKafkaListenerContainerFactory",
            batch = "true"
    )
    public void consume(List<Application> applicationList) {
        log.info("Get applications: {}", applicationList);
        applicationInfoService.saveAll(applicationList.stream()
                .map(mapper::mapToEntity)
                .toList()
        );
    }

}
