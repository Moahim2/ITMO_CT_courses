package com.academy.fintech.origination.grpc.application.v1;

import com.academy.fintech.application.ApplicationRequest;
import com.academy.fintech.application.CancelRequest;
import com.academy.fintech.common.grpc.DtoCreatingException;
import com.academy.fintech.origination.grpc.application.v1.dto.ApplicationDto;
import com.academy.fintech.origination.grpc.application.v1.dto.CancelDto;

import java.util.regex.Pattern;

import static com.academy.fintech.common.grpc.util.DtoMapperUtil.checkCorrectPrefixAndUUIDSuffix;

/**
 * Service for static (without going to DB etc.) checking of incoming requests for syntactic correctness.
 */
public class DtoMapper {
    private static final String EMAIL_PATTERN =
            "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$";

    private static final Pattern pattern = Pattern.compile(EMAIL_PATTERN);

    /**
     * Mapping incoming {@code request} to dto.
     *
     * @param request request.
     * @return dto if all input is syntax correct for next processing.
     * @throws DtoCreatingException if email isn't syntax correct (of pattern {@value #EMAIL_PATTERN}) or
     * {@code salary < 0} or {@code disbursementAmount < 0}.
     */
    public static ApplicationDto mapRequestToDto(ApplicationRequest request) throws DtoCreatingException {
        ApplicationDto applicationDto = ApplicationDto.fromRequest(request);

        if (!checkCorrectEmailSyntax(applicationDto.email())) {
            throw new DtoCreatingException(
                    "Incorrect email syntax. Email is matching in pattern %s.".formatted(EMAIL_PATTERN)
            );
        }

        if (checkNegativityNumber(applicationDto.salary())) {
            throw new DtoCreatingException("Incorrect salary. Salary must be positive.");
        }

        if (checkNegativityNumber(applicationDto.disbursementAmount())) {
            throw new DtoCreatingException("Incorrect disbursementAmount. Amount must be positive.");
        }

        return applicationDto;
    }

    /**
     * Mapping incoming {@code request} to dto.
     *
     * @param request request.
     * @return dto if all input is syntax correct for next processing.
     * @throws DtoCreatingException if applicationId isn't in format {@code "APP-<uuid>"}.
     */
    public static CancelDto mapRequestToDto(CancelRequest request) throws DtoCreatingException {
        CancelDto cancelDto = CancelDto.fromRequest(request);
        checkCorrectPrefixAndUUIDSuffix("APP-", cancelDto.applicationId(), "applicationId");
        return cancelDto;
    }

    private static boolean checkNegativityNumber(int value) {
        return value < 0;
    }

    private static boolean checkCorrectEmailSyntax(String email) {
        return pattern.matcher(email).matches();
    }
}
