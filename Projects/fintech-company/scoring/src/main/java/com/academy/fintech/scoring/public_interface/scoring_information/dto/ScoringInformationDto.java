package com.academy.fintech.scoring.public_interface.scoring_information.dto;

import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.common.grpc.util.ConverterUtil;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.List;

@Builder
public record ScoringInformationDto(BigDecimal periodPayment, List<LocalDate> overdueDates) {

    /**
     * Maps response to dto
     * @param scoringInformationResponse response.
     * @return dto
     * @throws DtoCreatingException if incorrect bigDecimalValue.
     */
    public static ScoringInformationDto fromResponse(ScoringInformationResponse scoringInformationResponse)
            throws DtoCreatingException {
        try {
            return ScoringInformationDto.builder()
                    .periodPayment(ConverterUtil.convertToBigDecimal(scoringInformationResponse.getPeriodPayment()))
                    .overdueDates(scoringInformationResponse
                            .getOverdueDatesList()
                            .stream()
                            .map(LocalDate::parse)
                            .toList())
                    .build();
        } catch (IllegalArgumentException e) {
            throw new DtoCreatingException("Incorrect BigDecimal value in request: " + e.getMessage());
        } catch (DateTimeParseException e) {
            throw new DtoCreatingException("Incorrect data format in request: " + e.getMessage());
        }
    }
}
