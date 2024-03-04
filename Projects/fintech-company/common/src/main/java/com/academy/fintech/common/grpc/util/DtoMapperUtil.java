package com.academy.fintech.common.grpc.util;

import com.academy.fintech.common.grpc.DtoCreatingException;
import lombok.experimental.UtilityClass;

import java.util.UUID;

@UtilityClass
public class DtoMapperUtil {
    public static void checkCorrectPrefixAndUUIDSuffix(String prefix, String id, String name)
            throws DtoCreatingException {
        checkCorrectPrefix(prefix, id, name);
        checkCorrectUUIDSuffix(prefix.length(), id, name);
    }

    public static void checkCorrectPrefix(String prefix, String id, String name) throws DtoCreatingException {
        if (!id.startsWith(prefix)) {
            throw new DtoCreatingException("Incorrect %s. Correct %s starts with %s".formatted(name, name, prefix));
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    public static void checkCorrectUUIDSuffix(int prefixLength, String id, String name) throws DtoCreatingException {
        try {
            UUID.fromString(id.substring(prefixLength));
        } catch (IllegalArgumentException e) {
            throw new DtoCreatingException("Incorrect %s uuid suffix: %s".formatted(name, e.getMessage()));
        }
    }
}
