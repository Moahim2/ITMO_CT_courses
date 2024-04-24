package com.academy.fintech.kafka.core.service.exporter.puller.exporter;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.ExportTaskService;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportStatus;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity.ExportTaskEntity;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.core.KafkaTemplate;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.concurrent.TimeUnit.MILLISECONDS;

/**
 * Exporter for working with task with string id.
 * Exports content to specify kafka's topic.
 *
 * @param <E> entity from db table.
 * @param <C> the final content to send to kafka.
 */
@Slf4j
@AllArgsConstructor
public abstract class AbstractSimpleExporter<E, C> implements Exporter {
    private final ExportTaskService exportTaskService;
    private final KafkaTemplate<String, C> kafkaTemplate;
    private final ConcurrentHashMap<Long, Boolean> exportTaskResultById = new ConcurrentHashMap<>();

    protected final String topicName;

    /**
     * {@inheritDoc}
     * Try to send (with butch limit) real entity from task's string ids to kafka topic.
     * After trying to send tasks try to set status of all good sending tasks to {@link ExportStatus#SUCCESS}.
     */
    @Override
    public void export() {
        List<ExportTaskEntity> tasks = exportTaskService.findByTypeAndStatusNew(getType());

        Map<String, ExportTaskEntity> agreementIdsToTaskId = tasks.stream()
                .collect(Collectors.toMap(
                        ExportTaskEntity::getStringKey,
                        Function.identity(),
                        (i1, i2) -> i1.getUpdatedAt().isAfter(i2.getUpdatedAt()) ? i1 : i2
                ));

        CompletableFuture<?>[] futures = getEntitiesByIds(new ArrayList<>(agreementIdsToTaskId.keySet()))
                .stream()
                .map(entity -> ContentTask.<C>builder()
                        .taskId(agreementIdsToTaskId.get(mapEntityToId(entity)).getId())
                        .content(mapEntityToContent(entity))
                        .build())
                .map(this::trySendTask)
                .toArray(CompletableFuture[]::new);

        waitSendingEnd(CompletableFuture.allOf(futures), agreementIdsToTaskId.size());
        setStatusToSuccess(tasks);
    }

    protected abstract List<E> getEntitiesByIds(List<String> ids);

    protected abstract C mapEntityToContent(E entity);

    protected abstract String mapEntityToId(E entity);

    protected abstract String mapContentToId(C content);

    private void waitSendingEnd(CompletableFuture<Void> commonCompletableFuture, int countSendingElements) {
        final long awaitTime = countSendingElements * 180000L + 60000;

        try {
            commonCompletableFuture.get(awaitTime, MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            throw new RuntimeException(e);
        }
    }

    private void setStatusToSuccess(List<ExportTaskEntity> tasks) {
        exportTaskService.setStatusToSuccess(tasks.stream()
                .filter(exportTaskEntity -> exportTaskResultById.getOrDefault(exportTaskEntity.getId(), false))
                .toList());
        tasks.forEach(exportTaskEntity -> exportTaskResultById.remove(exportTaskEntity.getId()));
    }

    private CompletableFuture<?> trySendTask(ContentTask<C> task) {
        try {
            return kafkaTemplate
                    .send(topicName, mapContentToId(task.content()), task.content())
                    .thenAccept(result -> exportTaskResultById.put(task.taskId(), true))
                    .exceptionally(exception -> {
                        processSendingError(task, exception);
                        return null;
                    });
        } catch (Exception e) {
            processSendingError(task, e);
            return CompletableFuture.completedFuture(null);
        }
    }

    private void processSendingError(ContentTask<?> task, Throwable e) {
        log.info("Processing task with id {} finish with error ", task.taskId(), e);
        exportTaskResultById.put(task.taskId(), false);
    }

}
