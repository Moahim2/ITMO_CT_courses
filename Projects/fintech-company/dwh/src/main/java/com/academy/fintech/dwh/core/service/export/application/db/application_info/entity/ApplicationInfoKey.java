package com.academy.fintech.dwh.core.service.export.application.db.application_info.entity;

import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;

@Embeddable
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ApplicationInfoKey  implements Serializable {
    private LocalDate businessDate;
    private String applicationId;
    private Instant updatedAt;
}
