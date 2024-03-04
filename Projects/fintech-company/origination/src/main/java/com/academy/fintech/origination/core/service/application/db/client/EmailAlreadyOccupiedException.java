package com.academy.fintech.origination.core.service.application.db.client;

public class EmailAlreadyOccupiedException extends RuntimeException {

    public EmailAlreadyOccupiedException(String message) {
        super(message);
    }
}
