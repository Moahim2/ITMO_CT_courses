package com.academy.fintech.origination.core.service.application;

import com.academy.fintech.application.ApplicationResponse;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationService;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import com.academy.fintech.origination.core.service.application.db.client.EmailAlreadyOccupiedException;
import com.academy.fintech.origination.grpc.application.v1.dto.ApplicationDto;
import com.academy.fintech.origination.public_interface.check_application.dto.CheckApplicationDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ApplicationCreationService {
    private final ClientCreationService clientCreationService;
    private final ApplicationService applicationService;
    private final ApplicationScoringService applicationScoringService;

    /**
     * Creates application from dto and save to db.
     * Creates client too if it wasn't created in last time.
     * All applications after creating will be added to schedule
     * for asynchronously scoring and accepted (or rejected).
     *
     * @param applicationDto dto.
     * @return response with {@code applicationId}.
     * @throws EmailAlreadyOccupiedException if
     * client with other personal data, but with requested email has already been created.
     * @throws DuplicateException if a duplicate was found in the status {@code "NEW"}.
     */
    @Transactional
    public ApplicationResponse create(ApplicationDto applicationDto)
            throws EmailAlreadyOccupiedException, DuplicateException {
        String clientId = clientCreationService.create(applicationDto);
        String applicationId = createImpl(applicationDto, clientId);

        addApplicationScoringToSchedule(applicationDto, applicationId, clientId);

        return ApplicationResponse.newBuilder()
                .setApplicationId(applicationId)
                .build();
    }

    /**
     * Creates application by clientId and dto, and save to db.
     *
     * @param applicationDto dto.
     * @param clientId id.
     * @return applicationId.
     * @throws DuplicateException if a duplicate was found in the status {@code "NEW"}.
     */
    private String createImpl(ApplicationDto applicationDto, String clientId) throws DuplicateException {
        ApplicationEntity application = ApplicationEntity.builder()
                .clientId(clientId)
                .disbursementAmount(applicationDto.disbursementAmount())
                .status(ApplicationStatus.NEW)
                .build();
        checkDuplicationsAndIfThrow(application, clientId);

        return applicationService.create(application);
    }

    private void checkDuplicationsAndIfThrow(ApplicationEntity application, String clientId) throws DuplicateException {
        Optional<ApplicationEntity> duplicateApplicationOpt = applicationService.getDuplicateApplication(
                application,
                clientId
        );
        if (duplicateApplicationOpt.isPresent()) {
            throw new DuplicateException(
                    duplicateApplicationOpt.get(),
                    "Duplicate! The application has already been received"
            );
        }
    }

    /**
     * Form task (add dto to service) for scoring application.
     * Add task to {@link ApplicationScoringService} where it will
     * asynchronously scored and will accepted it if score is good else rejected.
     * Then in service if scoring passed without errors sends information about application status to client's email.
     *
     * @param applicationDto dto.
     * @param applicationId id of created application.
     * @param clientId id of client.
     */
    private void addApplicationScoringToSchedule(ApplicationDto applicationDto, String applicationId, String clientId) {
        applicationScoringService.addNewScoringTask(CheckApplicationDto.builder()
                .applicationId(applicationId)
                .clientId(clientId)
                .salary(applicationDto.salary())
                .disbursementAmount(applicationDto.disbursementAmount())
                .build());
    }
}
