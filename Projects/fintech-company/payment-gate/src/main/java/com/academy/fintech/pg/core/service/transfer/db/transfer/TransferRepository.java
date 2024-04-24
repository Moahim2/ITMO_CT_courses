package com.academy.fintech.pg.core.service.transfer.db.transfer;

import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferEntity;
import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferStatus;
import org.springframework.data.repository.CrudRepository;

import java.util.Optional;

public interface TransferRepository extends CrudRepository<TransferEntity, String> {

    default Optional<TransferEntity> updateByStatusAndGet(String transferId, TransferStatus newStatus) {
        Optional<TransferEntity> transferOpt = findById(transferId);
        if (transferOpt.isEmpty()) {
            return transferOpt;
        } else {
            TransferEntity transfer = transferOpt.get();
            transfer.setStatus(newStatus);
            return Optional.of(save(transfer));
        }
    }

    Iterable<TransferEntity> findAllByStatus(TransferStatus status);
}
