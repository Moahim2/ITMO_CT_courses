package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement;

import com.academy.fintech.kafka.core.service.exporter.puller.enumeration.ExportType;
import com.academy.fintech.kafka.core.service.exporter.puller.db.export_task.ExportTaskService;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.AbstractSimpleExporter;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.AgreementService;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity.AccountType;
import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity.AgreementEntity;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class AgreementExporter extends AbstractSimpleExporter<AgreementEntity, Agreement> {
    private final AgreementService agreementService;

    public AgreementExporter(ExportTaskService exportTaskService,
                             KafkaTemplate<String, Agreement> kafkaTemplate,
                             AgreementService agreementService,
                             @Value("${exporter.agreement.topic}") String topicName) {
        super(exportTaskService, kafkaTemplate, topicName);
        this.agreementService = agreementService;
    }

    @Override
    protected List<AgreementEntity> getEntitiesByIds(List<String> ids) {
        return agreementService.getByIds(ids);
    }

    @Override
    protected Agreement mapEntityToContent(AgreementEntity entity) {
        Map<AccountType, String> accountTypeToBalance = new HashMap<>();
        entity.getAccountEntities().forEach(accountEntity -> accountTypeToBalance.put(
                        accountEntity.getKey().getType(), accountEntity.getBalance().toString()
        ));

        return Agreement.builder()
                .id(entity.getId())
                .productCode(entity.getProductCode())
                .clientId(entity.getClientId())
                .loanTermInMonths(entity.getLoanTermInMonths())
                .principalAmount(entity.getPrincipalAmount().toString())
                .interest(entity.getInterest().toString())
                .originationAmount(entity.getOriginationAmount().toString())
                .status(entity.getStatus().name())
                .disbursementDate(mapLocalDateToString(entity.getDisbursementDate()))
                .nextPaymentDate(mapLocalDateToString(entity.getNextPaymentDate()))
                .overdueBalance(accountTypeToBalance.get(AccountType.OVERDUE))
                .mainBalance(accountTypeToBalance.get(AccountType.MAIN))
                .createdAt(entity.getCreatedAt().toString())
                .businessDate(entity.getLastBusinessDate().toString())
                .build();
    }

    @Override
    protected String mapEntityToId(AgreementEntity entity) {
        return entity.getId();
    }

    @Override
    protected String mapContentToId(Agreement content) {
        return content.id();
    }

    @Override
    public ExportType getType() {
        return ExportType.AGREEMENT;
    }

    private String mapLocalDateToString(LocalDate date) {
        return date == null ? null : date.toString();
    }

}


