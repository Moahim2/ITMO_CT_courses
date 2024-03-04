package com.academy.fintech.pe.grpc.agreement.v1.dto;

import com.academy.fintech.BigDecimalValue;
import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.common.grpc.util.ConverterUtil;
import com.academy.fintech.common.grpc.DtoCreatingException;
import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record AgreementDto(
        String clientId,
        int loanTermInMonths,
        BigDecimal disbursementAmount,
        BigDecimal originationAmount,
        BigDecimal principalAmount,
        BigDecimal interest,
        String productCode
) {

    /**
     * Creating dto from {@code request}.
     *
     * @param request request.
     * @return dto, where {@code principalAmount = originationAmount + disbursementAmount}
     * @throws DtoCreatingException if BigDecimal numbers are incorrectly passed in the request.
     */
    public static AgreementDto fromRequest(AgreementRequest request) throws DtoCreatingException {
        BigDecimal disbursementAmount = convertToBigDecimal(request.getDisbursementAmount());
        BigDecimal originationAmount = convertToBigDecimal(request.getOriginationAmount());
        return AgreementDto.builder()
                .clientId(request.getClientId())
                .loanTermInMonths(request.getLoanTerm())
                .disbursementAmount(disbursementAmount)
                .originationAmount(originationAmount)
                .principalAmount(calculatePrincipalAmount(disbursementAmount, originationAmount))
                .interest(convertToBigDecimal(request.getInterest()))
                .productCode(request.getProductName())
                .build();
    }

    private static BigDecimal convertToBigDecimal(BigDecimalValue decimalValue) throws DtoCreatingException {
        try {
            return ConverterUtil.convertToBigDecimal(decimalValue);
        } catch (IllegalArgumentException e) {
            throw new DtoCreatingException("Incorrect BigDecimal value in request: " + e.getMessage());
        }
    }

    private static BigDecimal calculatePrincipalAmount(BigDecimal disbursementAmount, BigDecimal originationAmount) {
        return disbursementAmount.add(originationAmount);
    }
}
