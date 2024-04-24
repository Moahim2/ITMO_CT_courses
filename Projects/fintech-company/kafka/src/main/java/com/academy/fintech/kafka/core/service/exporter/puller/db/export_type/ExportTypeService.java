package com.academy.fintech.kafka.core.service.exporter.puller.db.export_type;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTaskTypeInfo;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTypeEntity;
import com.academy.fintech.kafka.core.service.exporter.puller.dispatcher.property.ExportTaskRetryStalledProperty;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;


import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ExportTypeService {
    private final ExportTaskTypeInfoRepository exportTaskTypeInfoRepository;
    private final ExportTypeRepository exportTypeRepository;
    private final ExportTaskRetryStalledProperty retryStalledProperty;

    public ExportTypeEntity getTypeByCode(ExportType exportType) {
        return exportTypeRepository.findById(exportType).orElseThrow();
    }

    public List<ExportTaskTypeInfo> findExportTasks() {
        return exportTaskTypeInfoRepository.findExportTasks();
    }

    public void updateStalledToAnotherRetry() {
        List<Long> retriedTaskIds = exportTaskTypeInfoRepository.updateStalledToAnotherRetry(
                retryStalledProperty.passedMinutesInProcessingToRetry()
        );

        if (!retriedTaskIds.isEmpty()) {
            log.warn("Retried stalled export tasks: {}", retriedTaskIds);
        }
    }

    public void updateStalledToErrorStatus() {
        List<Long> failedTaskIds = exportTaskTypeInfoRepository.updateStalledToErrorStatus(
                retryStalledProperty.passedMinutesInProcessingToRetry()
        );

        if (!failedTaskIds.isEmpty()) {
            log.error("Export tasks moved to ERROR status: {}", failedTaskIds);
        }
    }

}
