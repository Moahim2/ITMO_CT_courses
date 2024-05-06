package com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info;

import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity.AgreementInfoKey;
import com.academy.fintech.dwh.core.service.export.agreement.db.agreement_info.entity.AgreementInfoEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AgreementInfoRepository extends JpaRepository<AgreementInfoEntity, AgreementInfoKey> {

}
