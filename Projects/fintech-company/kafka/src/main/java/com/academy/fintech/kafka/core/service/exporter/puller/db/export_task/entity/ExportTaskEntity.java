package com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.entity;

import com.academy.fintech.kafka.core.service.exporter.puller.db.export_type.entity.ExportTypeEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.SourceType;

import java.time.Instant;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "export_task")
public class ExportTaskEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @ManyToOne
    @JoinColumn(name = "type")
    private ExportTypeEntity type;

    @Enumerated(EnumType.STRING)
    private ExportStatus status;

    @Column(name = "string_key")
    private String stringKey;
    @Column(name = "number_key")
    private Long numberKey;
    @Column(name = "retries_count")
    private int retriesCount;
    @Column(name = "updated_at")
    @CreationTimestamp(source = SourceType.DB)
    private Instant updatedAt;
}
