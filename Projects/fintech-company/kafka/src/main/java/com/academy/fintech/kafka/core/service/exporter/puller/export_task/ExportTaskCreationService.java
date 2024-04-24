package com.academy.fintech.kafka.core.service.exporter.puller.export_task;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.ExportTaskService;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportStatus;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.ExportTypeService;
import com.academy.fintech.kafka.grpc.export_task.v1.dto.NewExportTaskDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ExportTaskCreationService {
    private final ExportTaskService exportTaskService;
    private final ExportTypeService exportTypeService;

    /**
     * Create and save in dto task with status {@link ExportStatus#NEW}.
     * @param dto dto.
     */
    public void createNewTask(NewExportTaskDto dto) {
        exportTaskService.createTask(ExportTaskEntity.builder()
                .type(exportTypeService.getTypeByCode(ExportType.valueOf(dto.type())))
                .stringKey(dto.stringKey())
                .numberKey(dto.numberKey())
                .status(ExportStatus.NEW)
                .build());
    }
}
