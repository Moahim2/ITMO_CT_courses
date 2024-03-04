package com.academy.fintech.pe.core.service.agreement.db.agreement;

import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class AgreementService {
    private final AgreementRepository agreementRepository;

    public String createAndGetId(AgreementEntity agreement) {
        return agreementRepository.save(agreement).getId();
    }

    public Optional<AgreementEntity> get(String id) {
        return agreementRepository.findById(id);
    }

    /**
     * Changes the status of an object and save its to db.
     * @param curAgreement agreement you need to change the status.
     * @param newStatus status.
     */
    public void setStatusAndSave(AgreementEntity curAgreement, AgreementStatus newStatus) {
        curAgreement.setStatus(newStatus);
        agreementRepository.save(curAgreement);
    }

    public List<AgreementEntity> getAllActiveAgreements(String clientId) {
        return agreementRepository.getAgreementEntitiesByClientIdAndStatus(clientId, AgreementStatus.ACTIVE);
    }
}
