package levitskiy.hw.model;

import io.netty.buffer.ByteBuf;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

/**
 * Data type directly written to the channel.
 * Encoded and decoded using converters.
 * @see levitskiy.hw.converter.PackedDataEncoder
 * @see levitskiy.hw.converter.PackedDataDecoder
 *
 * @param typeCommand type of command (as {@link CommandType#type()})
 * @param stringArguments some string representation of the protocol for passing command arguments.
 * @param otherData files (or potentially more complex structures containing them).
 */
public record PackedData(String typeCommand, String stringArguments, ByteBuf otherData) {}
