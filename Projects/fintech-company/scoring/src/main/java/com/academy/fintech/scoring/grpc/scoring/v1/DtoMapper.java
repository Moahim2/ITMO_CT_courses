package com.academy.fintech.scoring.grpc.scoring.v1;

import com.academy.fintech.scoring.CheckApplicationRequest;
import com.academy.fintech.scoring.grpc.scoring.v1.dto.CheckApplicationDto;
import lombok.experimental.UtilityClass;

@UtilityClass
public class DtoMapper {

    public static CheckApplicationDto mapToDto(CheckApplicationRequest request) {
        return CheckApplicationDto.builder()
                .clientId(request.getClientId())
                .salary(request.getSalary())
                .disbursementAmount(request.getDisbursementAmount())
                .build();
    }
}
