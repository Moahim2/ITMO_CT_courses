package com.academy.fintech.kafka.core.service.exporter.puller.db.export_task;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportStatus;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.List;

public interface ExportTaskRepository extends JpaRepository<ExportTaskEntity, Long> {

    @Query(value = "select * from export_task " +
            "where status = 'NEW' and type = :type " +
            "limit 1000 " +
            "for update skip locked ", nativeQuery = true)
    List<ExportTaskEntity> findByTypeAndStatus(@Param("type") String type);

    @Modifying
    @Transactional
    default List<ExportTaskEntity> findByTypeAndStatusAndUpdateStatusToProcessing(String type) {
        List<ExportTaskEntity> taskEntities = findByTypeAndStatus(type).stream()
                .peek(exportTaskEntity -> {
                    exportTaskEntity.setStatus(ExportStatus.PROCESSING);
                    exportTaskEntity.setUpdatedAt(Instant.now());
                })
                .toList();
        saveAll(taskEntities);
        return taskEntities;
    }

}
