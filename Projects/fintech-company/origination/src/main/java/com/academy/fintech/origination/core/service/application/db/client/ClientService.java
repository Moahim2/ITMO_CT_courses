package com.academy.fintech.origination.core.service.application.db.client;

import com.academy.fintech.origination.core.service.application.db.client.entity.ClientEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ClientService {
    private final ClientRepository clientRepository;

    /**
     * Try to save client to db.
     *
     * @param clientEntity client.
     * @return client's id.
     * @throws EmailAlreadyOccupiedException if
     * client with other personal data, but with requested email has already been created.
     */
    public String create(ClientEntity clientEntity) throws EmailAlreadyOccupiedException {
       Optional<ClientEntity> clientOpt = clientRepository.findByEmail(clientEntity.getEmail());
       if (clientOpt.isEmpty()) {
           return clientRepository.save(clientEntity).getId();
       } else {
           ClientEntity oldClientWithCurrentEmail = clientOpt.get();
           if (oldClientWithCurrentEmail.equals(clientEntity)) {
               return oldClientWithCurrentEmail.getId();
           } else {
               throw new EmailAlreadyOccupiedException(
                       "Client with other personal data, but with requested email has already been created."
               );
           }
       }
    }

    public Optional<ClientEntity> get(String clientId) {
        return clientRepository.findById(clientId);
    }
}
