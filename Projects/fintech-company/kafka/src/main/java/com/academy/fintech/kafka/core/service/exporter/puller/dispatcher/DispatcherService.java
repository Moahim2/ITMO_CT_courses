package com.academy.fintech.kafka.core.service.exporter.puller.dispatcher;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.Exporter;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTaskTypeInfo;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.ExportTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.concurrent.TimeUnit.SECONDS;

@Slf4j
@Service
public class DispatcherService {
    private final ExportTypeService exportTypeService;
    private final Map<ExportType, Exporter> exporters;

    public DispatcherService(ExportTypeService exportTypeService, List<Exporter> exporters) {
        this.exportTypeService = exportTypeService;

        this.exporters = exporters.stream()
                .collect(Collectors.toMap(Exporter::getType, Function.identity()));
    }

    @Scheduled(fixedRateString = "${exporter.dispatcher.stalled-seconds-rate}", initialDelayString = "5", timeUnit = SECONDS)
    public void processStalled() {
        exportTypeService.updateStalledToAnotherRetry();
        exportTypeService.updateStalledToErrorStatus();
    }

    @Scheduled(fixedRateString = "${exporter.dispatcher.export-seconds-rate}", initialDelayString = "5", timeUnit = SECONDS)
    public void exporter() {
        List<ExportTaskTypeInfo> exportTasksToProcess = exportTypeService.findExportTasks();

        exportTasksToProcess.stream()
                .filter(task -> task.countToProcess() > 0)
                .forEach(task -> executeExporter(task.exportType()));
    }

    private void executeExporter(ExportType type) {
        exporters.get(type).export();
    }

}
