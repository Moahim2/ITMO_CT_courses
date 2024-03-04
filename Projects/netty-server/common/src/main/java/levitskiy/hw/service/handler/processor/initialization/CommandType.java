package levitskiy.hw.service.handler.processor.initialization;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for reflection on start application.
 * It reflects the bijection between the string type of the command and the processor.
 * This annotation is meant to be used in pairs for the processor in the server
 * and all processors in the client.
 * All classes annotated with this annotation
 * for correct reflexive substitution during program startup
 * must have a public constructor without arguments.
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface CommandType {
    String type();
}
