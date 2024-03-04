package com.academy.fintech.origination.core.service.application;

import com.academy.fintech.origination.core.service.application.db.client.EmailAlreadyOccupiedException;
import com.academy.fintech.origination.core.service.application.db.client.ClientService;
import com.academy.fintech.origination.core.service.application.db.client.entity.ClientEntity;
import com.academy.fintech.origination.grpc.application.v1.dto.ApplicationDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class ClientCreationService {
    private final ClientService clientService;

    /**
     * Try to create client from dto of application and save to db.
     * If the requested client's personal data completely matches the existing one, his id will be returned.
     *
     * @param applicationDto dto.
     * @return client's id.
     * @throws EmailAlreadyOccupiedException if
     * client with other personal data, but with requested email has already been created.
     */
    @Transactional
    public String create(ApplicationDto applicationDto) throws EmailAlreadyOccupiedException {
        ClientEntity client = ClientEntity.builder()
                .firstName(applicationDto.firstName())
                .lastName(applicationDto.lastName())
                .email(applicationDto.email())
                .salary(applicationDto.salary())
                .build();
        return clientService.create(client);
    }
}
