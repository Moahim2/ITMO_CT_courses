package com.academy.fintech.origination.grpc.integrate_test.db.application.entity;

import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

@Entity
@Table(name = "application")
public class TestApplicationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    public String id;

    @Column(name = "client_id")
    public String clientId;

    @Column(name = "requested_disbursement_amount")
    public int disbursementAmount;

    @Enumerated(value = EnumType.STRING)
    @Column(name = "status")
    public ApplicationStatus status;
}
