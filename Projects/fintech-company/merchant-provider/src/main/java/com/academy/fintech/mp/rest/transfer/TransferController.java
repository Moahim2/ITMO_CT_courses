package com.academy.fintech.mp.rest.transfer;

import com.academy.fintech.mp.core.service.transfer.TransferService;
import com.academy.fintech.mp.core.service.transfer.exception.TransferException;
import com.academy.fintech.mp.rest.transfer.info.CheckTransferRequest;
import com.academy.fintech.mp.rest.transfer.info.CheckTransferResponse;
import com.academy.fintech.mp.rest.transfer.info.ExecuteTransferRequest;
import com.academy.fintech.mp.rest.transfer.info.ExecuteTransferResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.concurrent.ExecutionException;

@Slf4j
@RequiredArgsConstructor
@RestController
@RequestMapping("/mp/transfer")
public class TransferController {
    private final TransferService transferService;
    private final TransferMapper transferMapper;

    /**
     * Receives the amount of money and forms and begins to carry out their transfer to the client.
     *
     * @param executeTransferRequest request.
     * @return response with id of transfer.
     */
    @PostMapping("execute")
    public ExecuteTransferResponse execute(@RequestBody ExecuteTransferRequest executeTransferRequest) {
        log.info("Got request: {}", executeTransferRequest);
        return transferMapper.mapIdToResponse(
                transferService.executeTransfer(
                        transferMapper.mapRequestToDto(executeTransferRequest)
                )
        );
    }

    /**
     * Checks completion condition of transfer.
     *
     * @param checkTransferRequest request.
     * @return response with information about condition of transfer (is it completed and if so, its end date)
     */
    @PostMapping("cache/check")
    public CheckTransferResponse check(@RequestBody CheckTransferRequest checkTransferRequest) {
        log.info("Got request: {}", checkTransferRequest);
        try {
            return transferMapper.mapDtoToResponse(
                    transferService.checkTransfer(
                            transferMapper.mapRequestToDto(checkTransferRequest)
                    )
            );
        } catch (TransferException e) {
            return CheckTransferResponse.builder()
                    .isCompleted(false)
                    .responseCode(2)
                    .errorMessage(e.getMessage())
                    .build();
        } catch (ExecutionException | InterruptedException e) {
            return CheckTransferResponse.builder()
                    .isCompleted(false)
                    .responseCode(3)
                    .errorMessage(e.getMessage())
                    .build();
        }
    }
}
