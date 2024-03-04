package com.academy.fintech.pe.core.service.agreement.db.product.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@Entity
@Table(name = "product")
public class ProductEntity {
    @Id
    private String code;
    private int minLoanTerm;
    private int maxLoanTerm;
    private BigDecimal minPrincipalAmount;
    private BigDecimal maxPrincipalAmount;
    private BigDecimal minInterest;
    private BigDecimal maxInterest;
    private BigDecimal minOriginationAmount;
    private BigDecimal maxOriginationAmount;
}
