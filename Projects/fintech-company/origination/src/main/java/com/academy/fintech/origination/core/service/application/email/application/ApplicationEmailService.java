package com.academy.fintech.origination.core.service.application.email.application;

import com.academy.fintech.origination.core.service.application.db.client.entity.ClientEntity;
import com.academy.fintech.origination.core.service.application.email.client.BasicEmailService;
import lombok.RequiredArgsConstructor;
import org.springframework.mail.MailException;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ApplicationEmailService {
    private static final String SUBJECT_APPLICATION = "CREDIT_APPLICATION";
    private static final String FORMAT_ANSWER_APPLICATION = "Dear client%s %s, your loan application has been %s!";
    private static final String ANSWER_ACCEPTED_APPLICATION = "ACCEPTED";
    private static final String ANSWER_REJECTED_APPLICATION = "REJECTED";

    private final BasicEmailService basicEmailService;

    public final void sendMsgAboutAppAccepted(ClientEntity client) throws MailException {
        sendMsgAboutAppStatus(client, ANSWER_ACCEPTED_APPLICATION);
    }

    public final void sendMsgAboutAppRejected(ClientEntity client) throws MailException {
        sendMsgAboutAppStatus(client, ANSWER_REJECTED_APPLICATION);
    }

    private void sendMsgAboutAppStatus(ClientEntity client, String answer) throws MailException {
        String mes = FORMAT_ANSWER_APPLICATION.formatted(client.getFirstName(), client.getLastName(), answer);
        basicEmailService.sendSimpleEmail(client.getEmail(), SUBJECT_APPLICATION, mes);
    }

}
