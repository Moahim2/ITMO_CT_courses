package com.academy.fintech.pe.grpc.agreement.v1;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.pe.grpc.agreement.v1.dto.AgreementDto;
import com.academy.fintech.pe.grpc.agreement.v1.dto.DisbursementDto;
import lombok.experimental.UtilityClass;

import static com.academy.fintech.common.grpc.util.DtoMapperUtil.checkCorrectPrefixAndUUIDSuffix;

/**
 * Service for static (without going to DB etc.) checking of incoming requests for syntactic correctness.
 */
@UtilityClass
public final class DtoMapper {

    /**
     * Mapping incoming {@code request} to dto
     *
     * @param request request.
     * @return dto if input correct.
     * @throws DtoCreatingException if clientId doesn't start with {@code "CLI-"}
     * or any field doesn't convert to the desired type.
     */
    public static AgreementDto createAgreementDto(AgreementRequest request) throws DtoCreatingException {
        AgreementDto agreementDto = AgreementDto.fromRequest(request);
        checkCorrectPrefixAndUUIDSuffix("CLI-", agreementDto.clientId(), "clientId");
        return agreementDto;
    }

    /**
     * Mapping incoming {@code request} to dto
     *
     * @param request request.
     * @return dto if input correct.
     * @throws DtoCreatingException if agreementId isn't in format {@code "L-<uuid>"}
     * or any field doesn't convert to the desired type.
     */
    public static DisbursementDto createDisbursementDto(DisbursementRequest request) throws DtoCreatingException {
        DisbursementDto disbursementDto = DisbursementDto.fromRequest(request);
        checkCorrectPrefixAndUUIDSuffix("L-", disbursementDto.agreementId(), "agreementId");
        return disbursementDto;
    }
}
