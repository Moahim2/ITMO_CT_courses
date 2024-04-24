package com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "export_type")
public class ExportTypeEntity {
    @Id
    @Enumerated(EnumType.STRING)
    private ExportType code;

    private String description;
    @Column(name = "retry_count")
    private int retryCount;
}
