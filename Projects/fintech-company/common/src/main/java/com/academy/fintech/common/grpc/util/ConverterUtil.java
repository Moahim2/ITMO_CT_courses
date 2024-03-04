package com.academy.fintech.common.grpc.util;

import com.academy.fintech.BigDecimalValue;
import com.google.protobuf.ByteString;
import lombok.experimental.UtilityClass;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

@UtilityClass
public class ConverterUtil {

    /**
     * Convert protobuf transmitted structure to real number.
     *
     * @param decimalValue protobuf's entity.
     * @return corresponding value
     * @throws IllegalArgumentException if {@code decimalValue} not number.
     */
    public static BigDecimal convertToBigDecimal(BigDecimalValue decimalValue) throws IllegalArgumentException {
        return new BigDecimal(
                new BigInteger(decimalValue.getValue().toByteArray()),
                decimalValue.getScale(),
                new MathContext(decimalValue.getPrecision())
        );
    }

    /**
     * Convert real value to message.
     *
     * @param bigDecimal value.
     * @return protobuf transmitted structure
     */
    public static BigDecimalValue convertToBigDecimalValue(BigDecimal bigDecimal) {
        return BigDecimalValue.newBuilder()
                .setScale(bigDecimal.scale())
                .setPrecision(bigDecimal.precision())
                .setValue(ByteString.copyFrom(bigDecimal.unscaledValue().toByteArray()))
                .build();
    }
}
