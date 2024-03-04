package com.academy.fintech.origination.core.service.application;

import com.academy.fintech.origination.core.service.application.db.application.entity.ApplicationEntity;
import lombok.Getter;

@Getter
public class DuplicateException extends Exception {
    private final String applicationId;

    public DuplicateException(ApplicationEntity application, String message) {
        super(message);
        this.applicationId = application.getId();
    }
}
