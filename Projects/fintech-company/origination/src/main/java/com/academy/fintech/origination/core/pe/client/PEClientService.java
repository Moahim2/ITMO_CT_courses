package com.academy.fintech.origination.core.pe.client;

import com.academy.fintech.agreement.AgreementRequest;
import com.academy.fintech.agreement.AgreementResponse;
import com.academy.fintech.agreement.DisbursementRequest;
import com.academy.fintech.agreement.DisbursementResponse;
import com.academy.fintech.origination.core.pe.client.grpc.PEGrpcClient;
import com.academy.fintech.origination.grpc.transfer.v1.dto.NotifyCompletionTransferDto;
import com.academy.fintech.origination.public_interface.agreement.dto.AgreementDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static com.academy.fintech.common.grpc.util.ConverterUtil.convertToBigDecimalValue;

@Slf4j
@Service
@RequiredArgsConstructor
public class PEClientService {
    private final PEGrpcClient peGrpcClient;

    /**
     * Create agreement in PE.
     *
     * @return agreementId if the creation was successful
     * else empty.
     */
    public Optional<String> create(AgreementDto dto) {
       AgreementResponse response = peGrpcClient.create(mapDtoToRequest(dto));
       log.info("Got response: {}", response);
       if (response.getResponseCode() == 0) {
           return Optional.of(response.getAgreementId());
       } else {
           log.warn("In pe was error: " + response.getErrorMessage());
           return Optional.empty();
       }
    }

    /**
     * Activate agreement with agreementId in pe.
     *
     * @return paymentScheduleId if the activation was successful
     * else empty.
     */
    public Optional<String> activate(NotifyCompletionTransferDto dto) {
        DisbursementResponse response = peGrpcClient.activate(mapDtoToRequest(dto));
        log.info("Got response: {}", response);
        if (response.getResponseCode() == 0) {
            return Optional.of(response.getPaymentScheduleId());
        } else {
            log.warn("In pe was error: " + response.getErrorMessage());
            return Optional.empty();
        }
    }

    private static AgreementRequest mapDtoToRequest(AgreementDto dto) {
        return AgreementRequest.newBuilder()
                .setClientId(dto.clientId())
                .setLoanTerm(dto.loanTerm())
                .setDisbursementAmount(convertToBigDecimalValue(dto.disbursementAmount()))
                .setOriginationAmount(convertToBigDecimalValue(dto.originationAmount()))
                .setInterest(convertToBigDecimalValue(dto.interest()))
                .setProductName(dto.productName())
                .build();
    }

    private static DisbursementRequest mapDtoToRequest(NotifyCompletionTransferDto dto) {
        return DisbursementRequest.newBuilder()
                .setAgreementId(dto.agreementId())
                .setData(dto.date())
                .build();
    }
}
