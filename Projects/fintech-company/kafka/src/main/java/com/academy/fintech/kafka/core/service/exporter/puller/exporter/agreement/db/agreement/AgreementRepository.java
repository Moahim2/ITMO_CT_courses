package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity.AgreementEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AgreementRepository extends JpaRepository<AgreementEntity, String> {

}
