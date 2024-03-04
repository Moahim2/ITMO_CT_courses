package com.academy.fintech.origination.core.service.application.db.application;

import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ApplicationService {
    private final ApplicationRepository applicationRepository;

    /**
     * Try to save application and client (how subfield in application) to db.
     *
     * @param application entity.
     * @return applicationId if saving was ok.
     */
    public String create(ApplicationEntity application) {
        return applicationRepository.save(application).getId();
    }

    /**
     * Find duplicate of current {@code application} in db.
     * Relies in its work on the fact that all clients have a unique email.
     * Duplicate applications are considered to have the same emails and amounts.
     *
     * @param application application.
     * @return optional with entity if a duplicate exists
     * else empty.
     */
    public Optional<ApplicationEntity> getDuplicateApplication(ApplicationEntity application, String clientId) {
        int disbursementAmount = application.getDisbursementAmount();
        return applicationRepository.getApplicationEntitiesByStatusAndDisbursementAmountAndClientId(
                ApplicationStatus.NEW,
                disbursementAmount,
                clientId
        ).stream().findFirst();
    }

    public Optional<ApplicationEntity> get(String id) {
        return applicationRepository.findById(id);
    }

    public Optional<ApplicationEntity> setStatus(String clientId, ApplicationStatus newStatus) {
        return applicationRepository.findById(clientId).map(application -> setStatus(application, newStatus));
    }

    public ApplicationEntity setStatus(ApplicationEntity application, ApplicationStatus newStatus) {
        application.setStatus(newStatus);
        return applicationRepository.save(application);
    }

}
