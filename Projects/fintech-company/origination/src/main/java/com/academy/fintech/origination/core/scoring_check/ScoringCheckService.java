package com.academy.fintech.origination.core.scoring_check;

import com.academy.fintech.origination.public_interface.check_application.dto.CheckApplicationDto;
import com.academy.fintech.origination.core.scoring.client.ScoringClientService;
import io.grpc.StatusRuntimeException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ScoringCheckService {
    private final ScoringClientService scoringClientService;

    public int check(CheckApplicationDto checkApplicationDto) throws StatusRuntimeException {
        return scoringClientService.check(checkApplicationDto);
    }
}
