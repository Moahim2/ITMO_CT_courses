package com.academy.fintech.scoring.core;

public class ServiceInteractionError extends RuntimeException {
    public ServiceInteractionError(String message) {
        super(message);
    }
}
