package com.academy.fintech.dwh.core.service.export.application;

import com.academy.fintech.dwh.core.service.export.application.db.application_info.entity.ApplicationInfoEntity;
import com.academy.fintech.dwh.core.service.export.application.db.application_info.entity.ApplicationInfoKey;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.time.LocalDate;

@Component
public class EntityMapper {

    public ApplicationInfoEntity mapToEntity(Application application) {
        return ApplicationInfoEntity.builder()
                .key(ApplicationInfoKey.builder()
                        .businessDate(LocalDate.parse(application.businessDate()))
                        .applicationId(application.id())
                        .updatedAt(Instant.parse(application.updatedAt()))
                        .build())
                .clientId(application.clientId())
                .disbursementAmount(application.disbursementAmount())
                .status(application.status())
                .createdAt(Instant.parse(application.createdAt()))
                .build();
    }

}
