package com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.entity.ApplicationEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ApplicationRepository extends JpaRepository<ApplicationEntity, String> {

}
