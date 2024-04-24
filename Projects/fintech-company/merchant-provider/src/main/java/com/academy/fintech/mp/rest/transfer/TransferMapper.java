package com.academy.fintech.mp.rest.transfer;

import com.academy.fintech.mp.public_interface.transfer.dto.CheckTransferDto;
import com.academy.fintech.mp.public_interface.transfer.dto.ExecuteTransferDto;
import com.academy.fintech.mp.rest.transfer.info.CheckTransferRequest;
import com.academy.fintech.mp.rest.transfer.info.CheckTransferResponse;
import com.academy.fintech.mp.rest.transfer.info.ExecuteTransferRequest;
import com.academy.fintech.mp.rest.transfer.info.ExecuteTransferResponse;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
public class TransferMapper {

    public ExecuteTransferDto mapRequestToDto(ExecuteTransferRequest request) {
        return ExecuteTransferDto.builder()
                .amount(new BigDecimal(request.amount()))
                .build();
    }

    public CheckTransferDto mapRequestToDto(CheckTransferRequest request) {
        return CheckTransferDto.builder()
                .transferId(request.transferId())
                .build();
    }

    public ExecuteTransferResponse mapIdToResponse(String id) {
        return ExecuteTransferResponse.builder()
                .transferId(id)
                .build();
    }

    public CheckTransferResponse mapDtoToResponse(CheckTransferDto checkTransferDto) {
        return CheckTransferResponse.builder()
                .isCompleted(checkTransferDto.isCompleted())
                .date(checkTransferDto.date())
                .responseCode(0)
                .build();
    }
}
