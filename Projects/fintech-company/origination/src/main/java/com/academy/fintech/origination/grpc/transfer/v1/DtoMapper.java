package com.academy.fintech.origination.grpc.transfer.v1;

import com.academy.fintech.application.NotifyingCompletionRequest;
import com.academy.fintech.origination.grpc.transfer.v1.dto.NotifyCompletionTransferDto;
import org.springframework.stereotype.Component;

@Component
public class DtoMapper {

    public NotifyCompletionTransferDto mapRequestToDto(NotifyingCompletionRequest request) {
        return NotifyCompletionTransferDto.builder()
                .agreementId(request.getAgreementId())
                .date(request.getDate())
                .build();
    }
}
