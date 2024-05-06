package com.academy.fintech.dwh.core.service.export.application.db.application_info;

import com.academy.fintech.dwh.core.service.export.application.db.application_info.entity.ApplicationInfoEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ApplicationInfoService {
    private final ApplicationInfoRepository applicationInfoRepository;

    public void saveAll(List<ApplicationInfoEntity> applicationInfoEntityList) {
        applicationInfoRepository.saveAll(applicationInfoEntityList);
    }

}
