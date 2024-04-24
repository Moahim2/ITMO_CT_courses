package com.academy.fintech.kafka.core.service.exporter.puller.db.export_task;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportStatus;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ExportTaskService {
    private final ExportTaskRepository exportTaskRepository;

    /**
     * Save task to db (with generating unique id).
     *
     * @param exportTaskEntity entity (without id).
     */
    public void createTask(ExportTaskEntity exportTaskEntity) {
        exportTaskRepository.save(exportTaskEntity);
    }

    /**
     * Find (with some limit) tasks with status {@link ExportStatus#NEW} and {@code type}.
     * Then with locked sets there status to {@link ExportStatus#PROCESSING} and return.
     *
     * @param type type of task.
     * @return requested tasks.
     */
    public List<ExportTaskEntity> findByTypeAndStatusNew(ExportType type) {
        return exportTaskRepository.findByTypeAndStatusAndUpdateStatusToProcessing(type.name());
    }

    public void setStatusToSuccess(List<ExportTaskEntity> tasks) {
        tasks.forEach(task -> task.setStatus(ExportStatus.SUCCESS));
        exportTaskRepository.saveAll(tasks);
    }

}
