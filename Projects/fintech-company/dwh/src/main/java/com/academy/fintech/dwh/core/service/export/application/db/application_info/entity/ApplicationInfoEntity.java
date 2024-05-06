package com.academy.fintech.dwh.core.service.export.application.db.application_info.entity;

import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.Instant;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "application_info")
public class ApplicationInfoEntity {
    @EmbeddedId
    private ApplicationInfoKey key;

    private String clientId;
    private int disbursementAmount;
    private String status;
    private Instant createdAt;
}
