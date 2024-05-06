package com.academy.fintech.dwh.core.service.export.agreement;

import lombok.Builder;

@Builder
public record Agreement(
        String id,
        String productCode,
        String clientId,
        int loanTermInMonths,
        String principalAmount,
        String interest,
        String originationAmount,
        String status,
        String disbursementDate,
        String nextPaymentDate,
        String overdueBalance,
        String mainBalance,
        String createdAt,
        String updatedAt,
        String businessDate
) {}
