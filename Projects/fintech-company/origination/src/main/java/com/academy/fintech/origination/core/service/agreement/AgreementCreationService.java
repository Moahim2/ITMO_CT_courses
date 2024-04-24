package com.academy.fintech.origination.core.service.agreement;

import com.academy.fintech.origination.core.pe.client.PEClientService;
import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import com.academy.fintech.origination.public_interface.agreement.dto.AgreementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class AgreementCreationService {
    public static final int DEFAULT_LOAN_TERM = 12;
    public static final BigDecimal DEFAULT_ORIGINATION_AMOUNT = new BigDecimal(3000);
    public static final BigDecimal DEFAULT_INTEREST_PER_YEAR = BigDecimal.TEN;
    public static final String DEFAULT_PRODUCT_NAME = "CL1.0";

    private final PEClientService peClientService;

    public Optional<String> createAgreementAndGetId(ApplicationEntity application) {
        return peClientService.create(mapApplicationEntityToAgreementDto(application));
    }

    private AgreementDto mapApplicationEntityToAgreementDto(ApplicationEntity application) {
        return AgreementDto.builder()
                .clientId(application.getClientId())
                .loanTerm(DEFAULT_LOAN_TERM)
                .disbursementAmount(new BigDecimal(application.getDisbursementAmount()))
                .originationAmount(DEFAULT_ORIGINATION_AMOUNT)
                .interest(DEFAULT_INTEREST_PER_YEAR)
                .productName(DEFAULT_PRODUCT_NAME)
                .build();
    }
}
