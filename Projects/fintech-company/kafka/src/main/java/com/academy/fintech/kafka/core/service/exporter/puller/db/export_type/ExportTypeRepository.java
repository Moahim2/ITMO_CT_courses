package com.academy.fintech.kafka.core.service.exporter.puller.db.export_type;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ExportTypeRepository extends JpaRepository<ExportTypeEntity, ExportType> {

}
