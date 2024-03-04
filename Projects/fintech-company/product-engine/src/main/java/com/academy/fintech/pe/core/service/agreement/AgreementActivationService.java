package com.academy.fintech.pe.core.service.agreement;

import com.academy.fintech.agreement.DisbursementResponse;
import com.academy.fintech.pe.core.service.agreement.db.agreement.AgreementService;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementEntity;
import com.academy.fintech.pe.core.service.agreement.db.agreement.entity.AgreementStatus;
import com.academy.fintech.pe.grpc.agreement.v1.dto.DisbursementDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Nullable;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class AgreementActivationService {
    private static final AgreementStatus WAITING_STATUS = AgreementStatus.WAITING;

    private final AgreementService agreementService;
    private final PaymentScheduleCreationService paymentScheduleCreationService;

    /**
     * Activate agreement with id {@link DisbursementDto#agreementId()} and set its status in db.
     *
     * @param disbursementDto dto.
     * @return response
     * <ul>
     *      <li>if ok -> {@code code = 0} and {@code paymentScheduleId} </li>
     *      <li>if unsuitable request conditions -> {@code code = 2} and {@code errorMessage}</li>
     * </ul>
     */
    @Transactional
    public DisbursementResponse activateAgreement(DisbursementDto disbursementDto) {
        DisbursementResponse.Builder responseBuilder = DisbursementResponse.newBuilder();
        
        AgreementEntity agreement = getAgreementIfWaiting(responseBuilder, disbursementDto);
        if (agreement != null) {
            String paymentScheduleId = activateAgreementImpl(agreement, disbursementDto);
            responseBuilder.setPaymentScheduleId(paymentScheduleId);
        }
        return responseBuilder.build();
    }

    /**
     * Finds agreement in db and checks its status.
     * Write {@code errorMessage} end {@code responseCode} to builder
     * if agreement not found or its status not {@code "WAITING"}.
     *
     * @param responseBuilder empty builder.
     * @param disbursementDto dto.
     * @return agreement
     * if it was created and its status is {@code "WAITING"}
     * else null.
     */
    @Nullable
    private AgreementEntity getAgreementIfWaiting(DisbursementResponse.Builder responseBuilder,
                                                  DisbursementDto disbursementDto) {
        Optional<AgreementEntity> agreementOpt = agreementService.get(disbursementDto.agreementId());
        if (agreementOpt.isEmpty()) {
            putErrorMessageToBuilder(responseBuilder, "There is no agreement with such an id");
            return null;
        }
        AgreementEntity agreement = agreementOpt.get();

        AgreementStatus realStatus = agreement.getStatus();
        if (realStatus != WAITING_STATUS) {
            putErrorMessageToBuilder(
                    responseBuilder,
                    "Agreement status not %s but %s".formatted(WAITING_STATUS.name(), realStatus.name())
            );
            return null;
        }
        return agreement;
    }

    private static void putErrorMessageToBuilder(DisbursementResponse.Builder responseBuilder, String message) {
        responseBuilder
                .setResponseCode(2)
                .setErrorMessage(message);
    }

    /**
     * Activate is first disbursement, creating payments and schedule for it,
     * and setting payment fields in {@code agreement}.
     *
     * @param agreement agreement.
     * @return id of created schedule.
     */
    private String activateAgreementImpl(AgreementEntity agreement, DisbursementDto disbursementDto) {
        String paymentScheduleId = paymentScheduleCreationService.createSchedule(agreement, disbursementDto);
        setAgreementStatusToActive(agreement);
        return paymentScheduleId;
    }

    private void setAgreementStatusToActive(AgreementEntity agreement) {
        agreementService.setStatusAndSave(agreement, AgreementStatus.ACTIVE);
    }
}
