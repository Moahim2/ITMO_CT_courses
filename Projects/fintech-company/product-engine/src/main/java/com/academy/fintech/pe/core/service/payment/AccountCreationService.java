package com.academy.fintech.pe.core.service.payment;

import com.academy.fintech.pe.core.service.payment.db.account.AccountService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AccountCreationService {
    private final AccountService accountService;

    public void createAccountsForAgreement(String agreementId) {
        accountService.createAccountsOfAllTypes(agreementId);
    }
}
