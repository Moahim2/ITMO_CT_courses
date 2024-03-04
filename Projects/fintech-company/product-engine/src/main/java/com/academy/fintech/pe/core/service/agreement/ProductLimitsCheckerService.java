package com.academy.fintech.pe.core.service.agreement;

import com.academy.fintech.pe.core.service.agreement.db.product.ProductService;
import com.academy.fintech.pe.core.service.agreement.db.product.entity.ProductEntity;
import com.academy.fintech.pe.grpc.agreement.v1.dto.AgreementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ProductLimitsCheckerService {
    private final ProductService productService;

    public static final class ProductNotFoundException extends RuntimeException {
        public ProductNotFoundException() {
            super("Product with requested product_code not found.");
        }
    }

    /**
     * Checking for compliance with the limits of the instance before the model product.
     *
     * @param agreementDto example of product.
     * @return true if {@code agreementDto} correct in product limits.
     * @throws ProductNotFoundException if product wasn't found on {@link AgreementDto#productCode()}.
     */
    public boolean checkAllLimits(AgreementDto agreementDto) throws ProductNotFoundException {
        ProductEntity product = productService
                .getProduct(agreementDto.productCode())
                .orElseThrow(ProductNotFoundException::new);
        return checkLimit(
                product.getMinLoanTerm(),
                product.getMaxLoanTerm(),
                agreementDto.loanTermInMonths()
        ) && checkLimit(
                product.getMinPrincipalAmount(),
                product.getMaxPrincipalAmount(),
                agreementDto.principalAmount()
        ) && checkLimit(
                product.getMinInterest(),
                product.getMaxInterest(),
                agreementDto.interest()
        ) && checkLimit(
                product.getMinOriginationAmount(),
                product.getMaxOriginationAmount(),
                agreementDto.originationAmount()
        );
    }

    private static <T> boolean checkLimit(T min, T max, Comparable<T> value) {
        return value.compareTo(max) <= 0 && value.compareTo(min) >= 0;
    }
}
