package com.academy.fintech.pe.core.service.payment.db.account;

import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountCompositeKey;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountEntity;
import com.academy.fintech.pe.core.service.payment.db.account.entity.AccountType;
import org.springframework.data.repository.CrudRepository;

import java.math.BigDecimal;

public interface AccountRepository extends CrudRepository<AccountEntity, AccountCompositeKey> {

    default void addPaymentToAccount(BigDecimal paymentAmount, AccountCompositeKey key) {
        findById(key)
                .map(account -> {
                    account.setBalance(account.getBalance().add(paymentAmount));
                    return account;
                })
                .ifPresentOrElse(this::save, () -> {
                    throw new IllegalArgumentException();
                });
    }

    default AccountEntity getAccountByAgreementIdAndType(String agreementId, AccountType type) {
        return findById(AccountCompositeKey.builder()
                        .agreementId(agreementId)
                        .type(type)
                        .build()).orElseThrow();
    }
}
