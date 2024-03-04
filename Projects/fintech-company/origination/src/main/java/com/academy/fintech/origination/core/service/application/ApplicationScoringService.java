package com.academy.fintech.origination.core.service.application;

import com.academy.fintech.origination.core.scoring_check.ScoringCheckService;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationService;
import com.academy.fintech.origination.core.service.application.db.application.ApplicationStatus;
import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import com.academy.fintech.origination.core.service.application.db.client.ClientService;
import com.academy.fintech.origination.core.service.application.db.client.entity.ClientEntity;
import com.academy.fintech.origination.core.service.application.email.application.ApplicationEmailService;
import com.academy.fintech.origination.public_interface.check_application.dto.CheckApplicationDto;
import io.grpc.StatusRuntimeException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;

/**
 * Service, which asynchronously sends calculation requests to scoring according to a schedule.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ApplicationScoringService {
    private final ApplicationService applicationService;
    private final ClientService clientService;
    private final ScoringCheckService scoringCheckService;
    private final ApplicationEmailService applicationEmailService;

    private final Queue<CheckApplicationDto> queueChecks = new ConcurrentLinkedDeque<>();

    /**
     * Put dto to queue of tasks which waiting their asynchronously scheduling application scoring in this service.
     * @param checkApplicationDto dto.
     */
    public void addNewScoringTask(CheckApplicationDto checkApplicationDto) {
        queueChecks.add(checkApplicationDto);
    }

    /**
     * Asynchronously scheduled running task with scoring application, status change and
     * sends information about application status to client's email
     * if scoring passed without errors.
     * Change status to {@code "ACCEPT"} if score is ok
     * else change status to {@code "CLOSED"}.
     */
    @Async
    @Scheduled(fixedRate = 1000) // A more complex plan will be developed later
    protected void scoreAndSetStatusImpl() {
        CheckApplicationDto checkApplicationDto = queueChecks.poll();
        if (checkApplicationDto == null) {
            return;
        }
        
        ApplicationEntity application = applicationService
                .setStatus(checkApplicationDto.applicationId(), ApplicationStatus.SCORING)
                .orElseThrow(() -> new NoSuchElementException("The application data was not found!"));
        try {
            int score = scoringCheckService.check(checkApplicationDto);
            setStatusOnScoreAndSendMessageToEmail(score, application);
        } catch (StatusRuntimeException e) {
            // usually if no connect
            // at this stage, it is unclear what to do with this (it is said that we can do nothing)
            // so return started status.
            log.info("It is unclear what to do with this.", e);
            applicationService.setStatus(application, ApplicationStatus.NEW);
        }
    }

    private void setStatusOnScoreAndSendMessageToEmail(int score, ApplicationEntity application) {
        ClientEntity client = clientService.get(application.getClientId()).orElseThrow(() ->
                new NoSuchElementException("The client's data was not found!")
        );
        if (score > 0) {
            applicationService.setStatus(application, ApplicationStatus.ACCEPTED);
            // COMMENTED BECAUSE THE SERVICE AND HOST DOMAIN IS UNREALISTIC
            // applicationEmailService.sendMsgAboutAppAccepted(client);
        } else {
            applicationService.setStatus(application, ApplicationStatus.CLOSED);
            //applicationEmailService.sendMsgAboutAppRejected(client);
        }
    }
}
