package com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info;

import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity.AgreementInfoEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class AgreementInfoService {
    private final AgreementInfoRepository agreementInfoRepository;

    public void saveAll(List<AgreementInfoEntity> agreementInfoEntityList) {
        agreementInfoRepository.saveAll(agreementInfoEntityList);
    }

}
