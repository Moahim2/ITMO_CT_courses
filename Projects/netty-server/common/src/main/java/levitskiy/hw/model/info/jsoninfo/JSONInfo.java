package levitskiy.hw.model.info.jsoninfo;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.netty.buffer.ByteBuf;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.Info;
import levitskiy.hw.service.ServerClientInteractionException;

/**
 * {@inheritDoc}
 * Implements the json protocol.
 * Must guarantee a bijection between {@link PackedData#stringArguments()} and its own json substructure.
 */
public interface JSONInfo extends Info {
    ObjectMapper objectMapper = new ObjectMapper();

    @Override
    default PackedData toPackedData(String commandType) {
        return new PackedData(commandType, mapObjectToJSONString(this), otherDataEncodeToByteBuf());
    }

    default ByteBuf otherDataEncodeToByteBuf() {
        return null;
    }

    /**
     * Unpack json from {@link PackedData#stringArguments()} to specific class type of {@code T}.
     *
     * @param data a data packet containing json.
     * @param clazz type of result.
     * @return class containing full unpacking json (but perhaps not only).
     * @param <T> type of unpacked object.
     * @throws ServerClientInteractionException if json couldn't unpack.
     */
    static <T extends JSONInfo> T unpackedJSONObjectFromPackedDataArguments(PackedData data, Class<T> clazz)
            throws ServerClientInteractionException {
        try {
            return objectMapper.readValue(data.stringArguments(), clazz);
        } catch (JsonProcessingException e) {
            throw new ServerClientInteractionException("JSON decoding error.", e);
        }
    }

    private static String mapObjectToJSONString(Object object) {
        try {
            return objectMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new ServerClientInteractionException("JSON encoding error.", e);
        }
    }
}
