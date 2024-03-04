package com.academy.fintech.origination.core.service.application;

import com.academy.fintech.application.CancelResponse;
import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationService;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import com.academy.fintech.origination.grpc.application.v1.dto.CancelDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.NoSuchElementException;

@Service
@RequiredArgsConstructor
public class ApplicationCancellationService {
    private final ApplicationService applicationService;

    /**
     * Try to cancel of application with id from dto.
     * Sets the status of required application to {@code "CLOSED"}).
     *
     * @param cancelDto dto.
     * @return response with {@code flag == false} if application has already been accepted or closed
     * else with {@code flag == true}.
     * @throws NoSuchElementException if application with such id doesn't exist.
     */
    @Transactional
    public CancelResponse cancel(CancelDto cancelDto) throws NoSuchElementException {
        ApplicationEntity application = applicationService.get(cancelDto.applicationId()).orElseThrow(() ->
                new NoSuchElementException("There is no application with such an id.")
        );
        boolean isCanceled = cancelIfNotAccepted(application);

        return CancelResponse.newBuilder()
                .setIsCanceled(isCanceled)
                .build();
    }

    private boolean cancelIfNotAccepted(ApplicationEntity application) throws NoSuchElementException {
        ApplicationStatus curStatus = application.getStatus();
        if (curStatus == ApplicationStatus.NEW || curStatus == ApplicationStatus.SCORING) {
            application.setStatus(ApplicationStatus.CLOSED);
            applicationService.create(application);
            return true;
        }
        return false;
    }
}
