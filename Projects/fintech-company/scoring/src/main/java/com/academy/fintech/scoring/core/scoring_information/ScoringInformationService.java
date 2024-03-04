package com.academy.fintech.scoring.core.scoring_information;

import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.scoring.core.ServiceInteractionError;
import com.academy.fintech.scoring.core.product_engine.client.ProductEngineClientService;
import com.academy.fintech.scoring.grpc.scoring.v1.dto.CheckApplicationDto;
import com.academy.fintech.scoring.public_interface.scoring_information.dto.ScoringInformationDto;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ScoringInformationService {
    private final ProductEngineClientService productEngineClientService;

    /**
     * Sends a request to receive information about the client (and his agreements)
     * and his potentially period payment in product-engine.
     *
     * @param checkApplicationDto dto.
     * @return dto with information from product-engine.
     * @throws ServiceInteractionError if internal error of the service or communication problems.
     */
    public ScoringInformationDto getScoringInformation(CheckApplicationDto checkApplicationDto)
            throws ServiceInteractionError {
        ScoringInformationResponse response = productEngineClientService.getScoringInformation(checkApplicationDto);
        try {
            if (response.getResponseCode() == 0) {
                return ScoringInformationDto.fromResponse(response);
            } else {
                throw new ServiceInteractionError("Product-engine exception.");
            }
        } catch (DtoCreatingException e) {
            throw new ServiceInteractionError("Unexpected error in the interaction of services " + e.getMessage());
        }
    }
}
