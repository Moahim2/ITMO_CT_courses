package levitskiy.hw.service.processor;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

/**
 * Base interface for processing server response.
 * For the application to work correctly with a specific implementation of this interface,
 * implementation must be:
 * <ul>
 *      <li>annotated {@link CommandType} with specific uniq type</li>
 *      <li>
 *      unique type must match the annotation type
 *      of the corresponding client input processor {@link ClientInputProcessor}
 *      and request processor in Server.
 *      </li>
 *      <li>the implementation of this class must have a public constructor</li>
 *      <li>the public constructor must be without arguments</li>
 * </ul>
 */
public interface ClientResponseProcessor {
    void unpackAndProcessResponse(PackedData response, DisplayUI displayUI);
}
