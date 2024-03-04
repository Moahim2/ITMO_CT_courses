package com.academy.fintech.origination.core.service.application.db.application;

import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface ApplicationRepository extends CrudRepository<ApplicationEntity, String> {
    List<ApplicationEntity> getApplicationEntitiesByStatusAndDisbursementAmountAndClientId(
            ApplicationStatus status,
            int disbursementAmount,
            String clientId
    );
}
