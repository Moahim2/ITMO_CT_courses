package com.academy.fintech.dwh.core.service.export.agreement;

import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity.AgreementInfoEntity;
import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity.AgreementInfoKey;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;

@Component
public class EntityMapper {

    public AgreementInfoEntity mapToInfoEntity(Agreement agreement) {
        return AgreementInfoEntity.builder()
                .key(AgreementInfoKey.builder()
                        .businessDate(LocalDate.parse(agreement.businessDate()))
                        .agreementId(agreement.id())
                        .updatedAt(Instant.parse(agreement.updatedAt()))
                        .build())
                .code(agreement.productCode())
                .clientId(agreement.clientId())
                .loanTerm(agreement.loanTermInMonths())
                .principalAmount(new BigDecimal(agreement.principalAmount()))
                .interest(new BigDecimal(agreement.interest()))
                .originationAmount(new BigDecimal(agreement.originationAmount()))
                .status(agreement.status())
                .disbursementDate(agreement.disbursementDate() == null ? null : LocalDate.parse(agreement.disbursementDate()))
                .nextPaymentDate(agreement.nextPaymentDate() == null ? null : LocalDate.parse(agreement.nextPaymentDate()))
                .overdueBalance(agreement.overdueBalance() == null ? null : new BigDecimal(agreement.overdueBalance()))
                .mainBalance(agreement.mainBalance() == null ? null : new BigDecimal(agreement.mainBalance()))
                .createdAt(Instant.parse(agreement.createdAt()))
                .build();
    }

}
