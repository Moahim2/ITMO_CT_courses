package com.academy.fintech.origination.core.service.application.db.application.entity;

import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.SourceType;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.Instant;
import java.time.LocalDate;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "application")
public class ApplicationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    String id;

    @Column(name = "client_id")
    String clientId;

    @Column(name = "requested_disbursement_amount")
    int disbursementAmount;

    @Enumerated(value = EnumType.STRING)
    @Column(name = "status")
    ApplicationStatus status;

    @CreationTimestamp(source = SourceType.DB)
    private Instant createdAt;
    @UpdateTimestamp(source = SourceType.DB)
    private LocalDate lastBusinessDate;
}
