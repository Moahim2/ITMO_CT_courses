package com.academy.fintech.origination.core.service.agreement;

import com.academy.fintech.origination.core.pe.client.PEClientService;
import com.academy.fintech.origination.grpc.transfer.v1.dto.NotifyCompletionTransferDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class AgreementActivationService {
    private final PEClientService peClientService;

    /**
     * Activate agreement with agreementId in pe.
     */
    public void activate(NotifyCompletionTransferDto dto) {
        Optional<String> paymentScheduleId = peClientService.activate(dto);
        if (paymentScheduleId.isEmpty()) {
            log.info("Agreement with id %s didn't create in pe.".formatted(dto.agreementId()));
        } else {
            paymentScheduleIdProcessing(paymentScheduleId.get());
        }
    }

    private void paymentScheduleIdProcessing(String paymentScheduleId) {
        // possible options for actions with the received ID of the scheme
        // have not yet been worked out and are not required.
    }
}
