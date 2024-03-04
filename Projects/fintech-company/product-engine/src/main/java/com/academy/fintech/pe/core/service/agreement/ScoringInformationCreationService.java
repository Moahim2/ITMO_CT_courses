package com.academy.fintech.pe.core.service.agreement;

import com.academy.fintech.pe.core.service.agreement.db.agreement.AgreementService;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule.PaymentScheduleService;
import com.academy.fintech.pe.core.service.agreement.db.payment_schedule_payment.entity.PaymentSchedulePaymentEntity;
import com.academy.fintech.pe.grpc.scoring_information.v1.dto.InformationDto;
import com.academy.fintech.scoring_information.ScoringInformationResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import static com.academy.fintech.common.grpc.util.ConverterUtil.convertToBigDecimalValue;
import static com.academy.fintech.pe.core.service.agreement.util.LoanUtil.calculateDefaultPeriodPayment;

@Service
@RequiredArgsConstructor
public class ScoringInformationCreationService {
    private final AgreementService agreementService;
    private final PaymentScheduleService paymentScheduleService;

    /**
     * Calculated pmt of future payment_schedule and find data about client's current overdue.
     *
     * @param informationDto dto.
     * @return response containing the calculated pmt and overdue dates of active agreements of input client.
     */
    public ScoringInformationResponse createInformation(InformationDto informationDto) {
        List<PaymentSchedulePaymentEntity> overduePayments = getAllOverduePayments(informationDto);
        List<String> overdueDates = mapToStringFormatPaymentDates(overduePayments);
        BigDecimal periodPayment = calculateDefaultPeriodPayment(informationDto.disbursementAmount());

        return ScoringInformationResponse.newBuilder()
                .setPeriodPayment(convertToBigDecimalValue(periodPayment))
                .addAllOverdueDates(overdueDates)
                .build();
    }

    private List<PaymentSchedulePaymentEntity> getAllOverduePayments(InformationDto informationDto) {
        return agreementService
                .getAllActiveAgreements(informationDto.clientId())
                .stream()
                .flatMap(agreement -> paymentScheduleService
                        .getAllOverduePayments(agreement.getId())
                        .stream())
                .toList();
    }

    private static List<String> mapToStringFormatPaymentDates(List<PaymentSchedulePaymentEntity> overduePayments) {
        return overduePayments
                .stream()
                .map(PaymentSchedulePaymentEntity::getPaymentDate)
                .map(LocalDate::toString)
                .toList();
    }

}
