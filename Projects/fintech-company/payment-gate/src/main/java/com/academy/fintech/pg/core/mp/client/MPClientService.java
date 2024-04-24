package com.academy.fintech.pg.core.mp.client;

import com.academy.fintech.pg.core.mp.client.rest.MPRestClient;
import com.academy.fintech.pg.core.mp.client.rest.transfer.excep.TransferCheckException;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.CheckTransferRequest;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.CheckTransferResponse;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.ExecuteTransferRequest;
import com.academy.fintech.pg.grpc.transfer.v1.dto.ExecuteTransferDto;
import com.academy.fintech.pg.public_interface.transfer.dto.CheckTransferDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MPClientService {
    private final MPRestClient restClient;

    /**
     * Pulling transfer to merchant-provider and get from him id of it.
     *
     * @param executeTransferDto dto.
     * @return id of transfer.
     */
    public String executeTransfer(ExecuteTransferDto executeTransferDto) {
        return restClient.executeTransfer(mapDtoToRequest(executeTransferDto)).transferId();
    }

    /**
     * Get information about transfer from merchant-provider.
     *
     * @param checkTransferDto dto.
     * @return dto with information about transfer status.
     * @throws com.academy.fintech.pg.core.mp.client.rest.transfer.excep.TransferCheckException
     * if response contains not good response code.
     */
    public CheckTransferDto checkTransfer(CheckTransferDto checkTransferDto) throws TransferCheckException {
        CheckTransferResponse response = restClient.checkTransfer(mapDtoToRequest(checkTransferDto));
        log.info("Got response: {}", response);
        return mapResponseToDto(restClient.checkTransfer(mapDtoToRequest(checkTransferDto)));
    }

    private static ExecuteTransferRequest mapDtoToRequest(ExecuteTransferDto executeTransferDto) {
        return ExecuteTransferRequest.builder()
                .amount(executeTransferDto.amount())
                .build();
    }

    private static CheckTransferRequest mapDtoToRequest(CheckTransferDto checkTransferDto) {
        return CheckTransferRequest.builder()
                .transferId(checkTransferDto.transferId())
                .build();
    }

    private static CheckTransferDto mapResponseToDto(CheckTransferResponse checkTransferResponse) {
        return CheckTransferDto.builder()
                .isCompleted(checkTransferResponse.isCompleted())
                .date(checkTransferResponse.date())
                .build();
    }

}
