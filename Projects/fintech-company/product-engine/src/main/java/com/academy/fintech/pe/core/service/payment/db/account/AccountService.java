package com.academy.fintech.pe.core.service.payment.db.account;

import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountCompositeKey;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountEntity;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

@Service
@RequiredArgsConstructor
public class AccountService {
    private final AccountRepository accountRepository;

    public void createAccountsOfAllTypes(String agreementId) {
        List<AccountEntity> accounts = Arrays.stream(AccountType.values())
                .map(accountType -> AccountCompositeKey.builder()
                        .agreementId(agreementId)
                        .type(accountType)
                        .build())
                .map(key -> AccountEntity.builder()
                        .key(key)
                        .balance(BigDecimal.ZERO)
                        .build())
                .toList();
        accountRepository.saveAll(accounts);
    }

    public void addMoneyToMainAccount(BigDecimal paymentAmount, String agreementId) {
        accountRepository.addPaymentToAccount(
                paymentAmount,
                AccountCompositeKey.builder()
                        .agreementId(agreementId)
                        .type(AccountType.MAIN)
                        .build()
        );
    }

    public AccountEntity getMainAccount(String agreementId) {
        return accountRepository.getAccountByAgreementIdAndType(agreementId, AccountType.MAIN);
    }

    public AccountEntity getOverdueAccount(String agreementId) {
        return accountRepository.getAccountByAgreementIdAndType(agreementId, AccountType.OVERDUE);
    }

    public void save(AccountEntity account) {
        accountRepository.save(account);
    }
}
