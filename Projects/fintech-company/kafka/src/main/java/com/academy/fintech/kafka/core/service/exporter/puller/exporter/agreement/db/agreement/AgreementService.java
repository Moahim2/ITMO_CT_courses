package com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.agreement.db.agreement.entity.AgreementEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class AgreementService {
    private final AgreementRepository agreementRepository;

    public List<AgreementEntity> getByIds(List<String> ids) {
        return agreementRepository.findAllById(ids);
    }

}
