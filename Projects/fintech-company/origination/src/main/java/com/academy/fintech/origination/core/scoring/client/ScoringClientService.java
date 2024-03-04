package com.academy.fintech.origination.core.scoring.client;

import com.academy.fintech.origination.public_interface.check_application.dto.CheckApplicationDto;
import com.academy.fintech.origination.core.scoring.client.grpc.ScoringGrpcClient;
import com.academy.fintech.scoring.CheckApplicationRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ScoringClientService {
    private final ScoringGrpcClient scoringGrpcClient;

    public int check(CheckApplicationDto checkApplicationDto) {
        return scoringGrpcClient.check(mapDtoToRequest(checkApplicationDto)).getResult();
    }

    private static CheckApplicationRequest mapDtoToRequest(CheckApplicationDto dto) {
        return CheckApplicationRequest.newBuilder()
                .setClientId(dto.clientId())
                .setSalary(dto.salary())
                .setDisbursementAmount(dto.disbursementAmount())
                .build();
    }
}
