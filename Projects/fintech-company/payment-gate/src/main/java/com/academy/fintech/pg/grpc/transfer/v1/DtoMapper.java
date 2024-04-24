package com.academy.fintech.pg.grpc.transfer.v1;

import com.academy.fintech.application.TransferRequest;
import com.academy.fintech.pg.grpc.transfer.v1.dto.ExecuteTransferDto;
import org.springframework.stereotype.Component;

@Component
public class DtoMapper {

    public ExecuteTransferDto mapRequestToDto(TransferRequest request) {
        return ExecuteTransferDto.builder()
                .agreementId(request.getAgreementId())
                .amount(request.getAmount())
                .build();
    }
}
