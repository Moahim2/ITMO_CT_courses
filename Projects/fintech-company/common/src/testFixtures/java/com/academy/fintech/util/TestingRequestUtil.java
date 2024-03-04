package com.academy.fintech.util;

import lombok.experimental.UtilityClass;
import org.junit.jupiter.api.Assertions;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.academy.fintech.util.RandomUtil.genRandomInt;

@UtilityClass
public class TestingRequestUtil {
    public static final String PREFIX_CLIENT_ID = "CLI-";
    public static final String PREFIX_AGREEMENT_ID = "L-";
    public static final String PREFIX_PAYMENT_SCHEDULE_ID = "PS-";
    public static final String PREFIX_APPLICATION_ID = "APP-";

    private static final String[] ALPHABET = new String[] {
            "a", "b", "c", "d", "e", "f", "g", "h", "i",
            "j", "k", "l", "m", "n", "o", "p", "q", "r",
            "s", "t", "u", "v", "w", "x", "y", "z"
    };

    private static final Set<String> ALREADY_GENERATED_EMAILS = new HashSet<>();

    public static String genRandomClientId() {
        return PREFIX_CLIENT_ID + genRandomUUID();
    }

    public static String genRandomAgreementId() {
        return PREFIX_AGREEMENT_ID + genRandomUUID();
    }

    public static String genRandomApplicationId() {
        return PREFIX_APPLICATION_ID + genRandomUUID();
    }

    public static String genRandomNewEmail() {
        int len = genRandomInt(3, 20);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < len; i++) {
            if (i == len / 2) {
                sb.append("@");
                continue;
            }

            String randChar;
            if (genRandomInt(0, 2) == 0) {
                randChar = ALPHABET[genRandomInt(0, ALPHABET.length)];
                if (genRandomInt(0, 2) == 0) {
                    randChar = randChar.toUpperCase();
                }
            } else {
                randChar = Integer.toString(genRandomInt(0, 11));
            }
            sb.append(randChar);
        }

        String res = sb.toString();
        if (ALREADY_GENERATED_EMAILS.add(res)) {
            return res;
        } else {
            return genRandomNewEmail();
        }
    }

    public static String genRandomEngName() {
        int len = genRandomInt(1, 20);
        return IntStream.range(0, len).mapToObj(i -> genRandomEngLetter()).collect(Collectors.joining());
    }

    private static String genRandomEngLetter() {
        return ALPHABET[genRandomInt(0, ALPHABET.length)];
    }

    private static String genRandomUUID() {
        return UUID.randomUUID().toString();
    }

    public static void checkCorrectUUIDWithPrefix(String prefix, String id) {
        Assertions.assertTrue(id.startsWith(prefix));
        Assertions.assertDoesNotThrow(() -> UUID.fromString(id.substring(prefix.length())));
    }
}
