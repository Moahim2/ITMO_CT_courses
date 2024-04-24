package com.academy.fintech.kafka.core.service.exporter.puller.db.export_type;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTaskTypeInfo;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface ExportTaskTypeInfoRepository extends JpaRepository<ExportTaskEntity, Long> {

    @Query(value = """
                with stalled as (
                    select task.id
                      from export_task task
                      join export_type type on type.code = task.type
                     where extract(epoch from clock_timestamp() - task.updated_at) / 60 >= :passedMinutesInProcessingToRetry
                       and task.status = 'PROCESSING'
                       and task.retries_count < type.retry_count
                )
                update export_task ue
                   set status = 'NEW',
                       updated_at = clock_timestamp(),
                       retries_count = retries_count + 1
                  from stalled se
                 where ue.id = se.id
                returning ue.id
                """, nativeQuery = true)
    List<Long> updateStalledToAnotherRetry(int passedMinutesInProcessingToRetry);

    @Query(value = """
                with stalled as (
                    select task.id
                      from export_task task
                      join export_type type on type.code = task.type
                     where extract(epoch from clock_timestamp() - task.updated_at) / 60 >= :passedMinutesInProcessingToRetry
                       and task.status = 'PROCESSING'
                       and task.retries_count >= type.retry_count
                )
                update export_task ue
                   set status = 'ERROR',
                       updated_at = clock_timestamp()
                  from stalled se
                 where ue.id = se.id
                returning ue.id
                """, nativeQuery = true)
    List<Long> updateStalledToErrorStatus(int passedMinutesInProcessingToRetry);

    @Query("select new " +
            " com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTaskTypeInfo(t.type.code, count(t.id))" +
            " from ExportTaskEntity t " +
            " inner join t.type et" +
            " where t.status = 'NEW' " +
            " group by t.type.code")
    List<ExportTaskTypeInfo> findExportTasks();

}
