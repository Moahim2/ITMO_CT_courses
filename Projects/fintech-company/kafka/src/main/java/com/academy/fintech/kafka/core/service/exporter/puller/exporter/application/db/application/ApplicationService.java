package com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application;

import com.academy.fintech.kafka.core.service.exporter.puller.exporter.application.db.application.entity.ApplicationEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ApplicationService {
    private final ApplicationRepository applicationRepository;

    public List<ApplicationEntity> getByIds(List<String> ids) {
        return applicationRepository.findAllById(ids);
    }

}
