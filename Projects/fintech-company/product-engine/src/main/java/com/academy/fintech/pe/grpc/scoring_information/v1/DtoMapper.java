package com.academy.fintech.pe.grpc.scoring_information.v1;

import com.academy.fintech.scoring_information.ScoringInformationRequest;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.pe.grpc.scoring_information.v1.dto.InformationDto;
import lombok.experimental.UtilityClass;

import static com.academy.fintech.common.grpc.util.DtoMapperUtil.checkCorrectPrefixAndUUIDSuffix;

@UtilityClass
public class DtoMapper {

    /**
     * Mapping incoming {@code request} to dto
     *
     * @param request request.
     * @return dto if input correct.
     * @throws DtoCreatingException if clientId isn't in format {@code "CLI-<uuid>"}
     */
    public static InformationDto createScoringDto(ScoringInformationRequest request) throws DtoCreatingException {
        InformationDto informationDto = InformationDto.fromRequest(request);
        checkCorrectPrefixAndUUIDSuffix("CLI-", informationDto.clientId(), "clientId");
        return informationDto;
    }
}
