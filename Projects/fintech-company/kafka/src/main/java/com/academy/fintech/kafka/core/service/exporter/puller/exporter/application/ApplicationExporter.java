package com.academy.fintech.kafka.core.service.exporter.puller.exporter.application;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.ExportTaskService;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.AbstractSimpleExporter;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.ApplicationService;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.entity.ApplicationEntity;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ApplicationExporter extends AbstractSimpleExporter<ApplicationEntity, Application> {
    private final ApplicationService applicationService;

    public ApplicationExporter(ExportTaskService exportTaskService,
                               KafkaTemplate<String, Application> kafkaTemplate,
                               ApplicationService applicationService,
                               @Value("${exporter.application.topic}") String topicName) {
        super(exportTaskService, kafkaTemplate, topicName);
        this.applicationService = applicationService;
    }

    @Override
    protected List<ApplicationEntity> getEntitiesByIds(List<String> ids) {
        return applicationService.getByIds(ids);
    }

    @Override
    protected Application mapEntityToContent(ApplicationEntity entity) {
        return Application.builder()
                .id(entity.getId())
                .clientId(entity.getClientId())
                .disbursementAmount(entity.getDisbursementAmount())
                .status(entity.getStatus().name())
                .createdAt(entity.getCreatedAt().toString())
                .businessDate(entity.getLastBusinessDate().toString())
                .build();
    }

    @Override
    protected String mapEntityToId(ApplicationEntity entity) {
        return entity.getId();
    }

    @Override
    protected String mapContentToId(Application content) {
        return content.id();
    }

    @Override
    public ExportType getType() {
        return ExportType.APPLICATION;
    }

}
