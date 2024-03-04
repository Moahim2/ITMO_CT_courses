package levitskiy.hw.model.info;

import levitskiy.hw.model.PackedData;

/**
 * Some unpacked view date for simple processor working.
 */
public interface Info {
    PackedData toPackedData(String commandType);
}
