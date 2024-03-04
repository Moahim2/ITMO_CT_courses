package com.academy.fintech.scoring.core.service.check_application;

import com.academy.fintech.scoring.CheckApplicationResponse;
import com.academy.fintech.scoring.core.ServiceInteractionError;
import com.academy.fintech.scoring.core.scoring_information.ScoringInformationService;
import com.academy.fintech.scoring.grpc.scoring.v1.dto.CheckApplicationDto;
import com.academy.fintech.scoring.public_interface.scoring_information.dto.ScoringInformationDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class ScoringCheckService {
    private final ScoringInformationService scoringInformationService;

    /**
     * Sends a request for information to product-engine and calculate result scoring.
     *
     * @param checkApplicationDto dto with information about client and his new application.
     * @return score.
     * @throws ServiceInteractionError if was unexpected errors with connection or other.
     */
    public CheckApplicationResponse checkApplication(CheckApplicationDto checkApplicationDto)
            throws ServiceInteractionError {
        ScoringInformationDto scoringInformation = scoringInformationService.getScoringInformation(checkApplicationDto);
        int score = ScoreCalculatedService.calcFullScore(scoringInformation, checkApplicationDto);

        log.info("SCORE = {}", score);

        return CheckApplicationResponse.newBuilder()
                .setResult(score)
                .build();
    }

}
