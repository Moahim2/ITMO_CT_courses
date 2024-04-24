package com.academy.fintech.kafka.core.service.exporter.puller.exporter;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTypeEntity;
import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;

public interface Exporter {

    /**
     * Export some tasks with type {@link #getType()} from export_task table to kafka topics.
     */
    void export();

    /**
     * @return type task of this exporter (type as code from export_type table).
     * @see ExportTypeEntity#getCode()
     */
    ExportType getType();

}
