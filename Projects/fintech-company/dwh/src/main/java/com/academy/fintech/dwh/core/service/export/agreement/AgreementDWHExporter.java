package com.academy.fintech.dwh.core.service.export.agreement;

import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.AgreementInfoService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class AgreementDWHExporter {
    private final AgreementInfoService agreementInfoService;
    private final EntityMapper entityMapper;

    /**
     * Consumes (with butch) all new agreement's states from kafka topic and save its to db.
     */
    @KafkaListener(
        topics = "${exporter.agreement.topic}",
        groupId = "dwh-agreement",
        containerFactory = "agreementConcurrentKafkaListenerContainerFactory",
        batch = "true"
    )
    public void consume(List<Agreement> agreementList) {
        log.info("Get agreements: {}", agreementList);
        agreementInfoService.saveAll(agreementList.stream()
                .map(entityMapper::mapToInfoEntity)
                .toList()
        );
    }

}
