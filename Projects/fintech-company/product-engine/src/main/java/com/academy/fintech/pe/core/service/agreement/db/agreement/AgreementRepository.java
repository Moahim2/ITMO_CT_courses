package com.academy.fintech.pe.core.service.agreement.db.agreement;

import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import org.springframework.data.repository.CrudRepository;

import java.util.List;

public interface AgreementRepository extends CrudRepository<AgreementEntity, String> {
    List<AgreementEntity> getAgreementEntitiesByClientIdAndStatus(String clientId, AgreementStatus status);
}
