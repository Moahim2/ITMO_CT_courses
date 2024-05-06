package com.academy.fintech.dwh.core.service.export.application.db.application_info;

import com.academy.fintech.dwh.core.service.export.application.db.application_info.entity.ApplicationInfoEntity;
import com.academy.fintech.dwh.core.service.export.application.db.application_info.entity.ApplicationInfoKey;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ApplicationInfoRepository extends JpaRepository<ApplicationInfoEntity, ApplicationInfoKey> {

}
