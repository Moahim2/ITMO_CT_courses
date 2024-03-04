package com.academy.fintech.pe.grpc.agreement;

import com.academy.fintech.common.grpc.DtoCreatingException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.function.Executable;

public class TestingDtoMapperUtil {

    public static void checkDtoCreatingException(Executable executable) {
        Assertions.assertThrows(DtoCreatingException.class, executable);
    }
}
