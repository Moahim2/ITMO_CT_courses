package com.academy.fintech.pg.core.service.transfer.db.transfer;

import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferEntity;
import com.academy.fintech.pg.core.service.transfer.db.transfer.entity.TransferStatus;
import com.academy.fintech.pg.grpc.transfer.v1.dto.ExecuteTransferDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class TransferService {
    private final TransferRepository transferRepository;

    public TransferEntity saveTransferAndGet(ExecuteTransferDto executeTransferDto, String transferId) {
        return transferRepository.save(TransferEntity.builder()
                .id(transferId)
                .agreementId(executeTransferDto.agreementId())
                .status(TransferStatus.EXECUTING)
                .build());
    }

    public Optional<String> setStatusToCompletedAndGetAgreementId(String transferId) {
        return transferRepository
                .updateByStatusAndGet(transferId, TransferStatus.COMPLETED)
                .map(TransferEntity::getAgreementId);
    }

    public void setStatusToRejected(String transferId) {
        transferRepository.updateByStatusAndGet(transferId, TransferStatus.REJECTED);
    }

    public Iterable<TransferEntity> getAllExecutingTransfers() {
        return transferRepository.findAllByStatus(TransferStatus.EXECUTING);
    }
}
