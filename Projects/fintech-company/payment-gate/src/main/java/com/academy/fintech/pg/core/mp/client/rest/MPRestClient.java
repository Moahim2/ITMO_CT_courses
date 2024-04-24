package com.academy.fintech.pg.core.mp.client.rest;

import com.academy.fintech.pg.core.mp.client.rest.transfer.excep.TransferCheckException;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.CheckTransferRequest;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.CheckTransferResponse;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.ExecuteTransferRequest;
import com.academy.fintech.pg.core.mp.client.rest.transfer.info.ExecuteTransferResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequiredArgsConstructor
@Slf4j
public class MPRestClient {
    private final RestTemplate restTemplate;

    public ExecuteTransferResponse executeTransfer(@RequestBody ExecuteTransferRequest executeTransferRequest) {
        String url = "mp/transfer/execute";
        return basePostForEntityAndGetBody(url, executeTransferRequest, ExecuteTransferResponse.class);
    }

    /**
     * Send request to merchant-provider about checking current transfer status.
     *
     * @param checkTransferRequest request.
     * @return response about transfer status.
     * @throws TransferCheckException if response contains not good response code.
     */
    public CheckTransferResponse checkTransfer(@RequestBody CheckTransferRequest checkTransferRequest)
            throws TransferCheckException {
        String url = "mp/transfer/cache/check";
        CheckTransferResponse response =
                basePostForEntityAndGetBody(url, checkTransferRequest, CheckTransferResponse.class);
        if (response.responseCode() != 0) {
            log.warn(response.errorMessage());
            throw new TransferCheckException(response.errorMessage());
        }
        return response;
    }

    private <K, V> V basePostForEntityAndGetBody(String url, K request, Class<V> responseClazz) {
        return restTemplate.postForEntity(
                url,
                new HttpEntity<>(request),
                responseClazz
        ).getBody();
    }
}
