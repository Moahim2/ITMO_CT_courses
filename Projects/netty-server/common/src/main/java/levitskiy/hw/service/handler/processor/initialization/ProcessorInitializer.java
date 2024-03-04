package levitskiy.hw.service.handler.processor.initialization;

import org.reflections.Reflections;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProcessorInitializer {
    /**
     * Constructs a map from {@link CommandType#type()} to a processor type of T
     * visible in the package of {@code interfaceClazz}.
     * @param interfaceClazz base class of processor.
     * @param <T> type of processors.
     */
    public static <T> Map<String, T> initAllImplementationOfInterfaceWithCommandTypeAnnotation(Class<T> interfaceClazz) {
        List<Class<? extends T>> list = new Reflections(interfaceClazz.getPackageName())
                .getSubTypesOf(interfaceClazz)
                .stream()
                .filter(ProcessorInitializer::isImplementedClassWithCommandTypeAnnotation)
                .toList();

        Map<String, T> resultMap = new HashMap<>();
        for (Class<? extends T> clazz : list) {
            try {
                Constructor<? extends T> constructor = clazz.getConstructor();
                resultMap.put(clazz.getAnnotation(CommandType.class).type(), constructor.newInstance());
            } catch (InvocationTargetException | InstantiationException | IllegalAccessException e) {
                System.err.println(
                        "Class " + clazz.getName() + " must contain only a public constructor" +
                                " without arguments for the correct configuration of the processor."
                );
            } catch (NoSuchMethodException e) {
                System.err.println(
                        "Class " + clazz.getName() + " does not contain a public constructor," +
                                " which is necessary for the correct configuration of the processor."
                );
            }
        }
        return resultMap;
    }

    private static boolean isImplementedClassWithCommandTypeAnnotation(Class<?> clazz) {
        int modifiers = clazz.getModifiers();
        return !Modifier.isAbstract(modifiers)
                && !Modifier.isInterface(modifiers)
                && clazz.getAnnotation(CommandType.class) != null;
    }
}
