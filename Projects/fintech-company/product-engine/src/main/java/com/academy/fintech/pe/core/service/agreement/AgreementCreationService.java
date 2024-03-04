package com.academy.fintech.pe.core.service.agreement;

import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.AgreementResponse.Builder;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.agreement.AgreementService;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import com.academy.fintech.pe.grpc.agreement.v1.dto.AgreementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Nullable;

@Service
@RequiredArgsConstructor
public class AgreementCreationService {
    private final ProductLimitsCheckerService productLimitsCheckerService;
    private final AgreementService agreementService;

    /**
     * Creates agreement from {@code dto} and save to db.
     *
     * @param agreementDto dto.
     * @return response
     * <ul>
     *      <li>if ok -> {@code code = 0} and {@code agreementId} </li>
     *      <li>if unsuitable request conditions -> {@code code = 2} and {@code errorMessage}</li>
     * </ul>
     */
    @Transactional
    public AgreementResponse create(AgreementDto agreementDto) {
        Builder responseBuilder = AgreementResponse.newBuilder();

        String agreementId = createImpl(agreementDto, responseBuilder);
        if (agreementId != null) {
            responseBuilder.setAgreementId(agreementId);
        }
        return responseBuilder.build();
    }

    /**
     * Creates agreement from {@code dto} and save to db.
     * Write {@code errorMessage} end {@code responseCode} to builder
     * if agreement parameters don't meet product limits.
     *
     * @param agreementDto dto
     * @param responseBuilder empty builder.
     * @return id of created agreement
     * if agreement was created and saved to db
     * else null.
     */
    @Nullable
    private String createImpl(AgreementDto agreementDto, Builder responseBuilder) {
        try {
            if (!productLimitsCheckerService.checkAllLimits(agreementDto)) {
                putErrorMessageToBuilder(
                        responseBuilder,
                        "The agreement cannot be created because the product limits are not met."
                );
                return null;
            }
        } catch (ProductLimitsCheckerService.ProductNotFoundException e) {
            putErrorMessageToBuilder(responseBuilder, e.getMessage());
            return null;
        }
        return createAndGetId(agreementDto);
    }

    private static void putErrorMessageToBuilder(Builder responseBuilder, String message) {
        responseBuilder
                .setResponseCode(2)
                .setErrorMessage(message);
    }

    private String createAndGetId(AgreementDto agreementDto) {
        return agreementService.createAndGetId(AgreementEntity.builder()
                .productCode(agreementDto.productCode())
                .clientId(agreementDto.clientId())
                .loanTermInMonths(agreementDto.loanTermInMonths())
                .principalAmount(agreementDto.principalAmount())
                .interest(agreementDto.interest())
                .originationAmount(agreementDto.originationAmount())
                .status(AgreementStatus.WAITING)
                .build()
        );
    }
}
