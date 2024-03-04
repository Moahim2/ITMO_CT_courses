package com.academy.fintech.pe.grpc.agreement.v1.dto;

import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.common.grpc.DtoCreatingException;
import lombok.Builder;

import java.time.LocalDate;
import java.time.format.DateTimeParseException;

@Builder
public record DisbursementDto(String agreementId, LocalDate disbursementDate) {

    /**
     * Creating dto from {@code request}.
     *
     * @param request request.
     * @return dto.
     * @throws DtoCreatingException if date not in format yyyy-mm-dd.
     */
    public static DisbursementDto fromRequest(DisbursementRequest request) throws DtoCreatingException {
        try {
            return DisbursementDto.builder()
                    .agreementId(request.getAgreementId())
                    .disbursementDate(LocalDate.parse(request.getData()))
                    .build();
        } catch (DateTimeParseException e) {
            throw new DtoCreatingException(e.getMessage());
        }
    }
}
