package info.kgeorgiy.ja.levitskii.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.util.Arrays;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;

/**
 * The Implementor class implements the {@link JarImpler} interface.
 * The Implementor can generate the implementation of interfaces (method {@link #implement(Class, Path)})
 * and at the same time generate and package it in a jar (method {@link #implementJar(Class, Path)}).
 * Result class simple name always (after using methods this class) is interface name with suffix {@code "Impl"}.
 * Result class is not abstract and full syntax correctly.
 * Method implementations always ignore arguments and return default values for all types.
 * Contains main {@link #main(String[])}, so may be run in cmd.
 *
 * @author Ivanl
 * @version 19
 */
public class Implementor implements JarImpler {
    /**
     * Value for single space is {@value}.
     */
    private static final String WS = " ";

    /**
     * Value for function tabulation (4 {@value #WS}) for best interface implementation design only.
     */
    private static final String TAB = WS.repeat(4);

    /**
     * Value {@value}
     * for naming the root of the temporary directory, which will always be deleted when the execution ends.
     */
    private static final String NAME_TEMP_DIR = "ImplementorTmpCacheDir";

    /**
     * Constant field for <var>.jar</var> packaging.
     * After initialization, it contains must contain one mandatory line -
     * the version {@code "1.0"} of the MANIFEST.mf ({@link Attributes.Name#MANIFEST_VERSION})
     */
    private static final Manifest MANIFEST = new Manifest();

    static {
        MANIFEST.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
    }

    /**
     * This is default empty constructor.
     */
    public Implementor() {}

    /**
     * Constant field only for recursively deleting temporary directory with the help
     * {@link Files#walkFileTree(Path, FileVisitor)}.
     * Implemented inside in a reasonable way,
     * deleting directories and files in during a visiting is equivalent to calling {@link Files#delete(Path)}.
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        //kgeorgiy's code
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Tries to create a full parent directory branch.
     * This method works in exactly the manner specified by {@link Files#createDirectories(Path p, FileAttribute[])}
     * when {@code p = path.getParent()}.
     *
     * @param path path to the directory to which you want to create a parent branch.
     * @throws ImplerException if an I/O error occurs.
     */
    private void createParentDir(Path path) throws ImplerException {
        try {
            Files.createDirectories(path.getParent());
        } catch (IOException e) {
            throw new ImplerException("Cannot create parent directories.", e);
        }
    }


    /**
     * Creates an interface implementation or
     * creates {@code jar} with interface implementations, intended for using {@code Implementor} on the command line.
     * It should act equivalent to the following:
     * <ul>
     *     <li>
     *         call method {@link #implement(Class clazz, Path path)} when {@code path = Path.of(".")}
     *         from {@code args = [clazzName]}.
     *     </li>
     *     <li>
     *         call method {@link #implement(Class clazz, Path path)}
     *         from {@code args = [clazzName, path]}.
     *     </li>
     *     <li>
     *         call method {@link #implementJar(Class, Path)}
     *         from {@code args = [-jar, clazzName, pathToJar]}.
     *     </li>
     * </ul>
     *
     * @param args array of arguments, which
     * for correct working may be only in 3 forms
     * <ul>
     *             <li>{@code [clazzName]}.</li>
     *             <li>{@code [clazzName, path]}.</li>
     *             <li>{@code [-jar, clazzName, pathToJar]}.</li>
     * </ul>
     * when {@code clazzName} - correct full name of the compiled class (only interface) visible in current classpath,
     * {@code path} - path to the directory where you want to get the implementation,
     * {@code pathToJar} - full name (with path) of result {@code .jar}
     */
    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Array of arguments is null.");
            return;
        }
        if (args.length == 0 || args.length > 3) {
            System.err.println("Bad count of arguments.");
            return;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Array of arguments contains null.");
            return;
        }
        final Implementor implementor = new Implementor();
        try {
            final Class<?> token = Class.forName(
                    Objects.equals(args[0], "-jar") && args.length > 1 ? args[1] : args[0]
            );
            if (args.length == 1) {
                implementor.implement(token, Path.of("."));
            } else if (args.length == 2) {
                implementor.implement(token, Path.of(args[1]));
            } else {
                implementor.implementJar(token, Path.of(args[2]));
            }
        } catch (ImplerException e) {
            System.out.println(e.getMessage());
        } catch (ClassNotFoundException e) {
            System.err.println("Bad class name. Class not found.");
            System.err.println(e.getMessage());
        } catch (InvalidPathException e) {
            System.err.println("Bad path.");
            System.err.println(e.getMessage());
        }
    }


    /**
     * Produces code implementing (only) interface specified by provided {@code token}.
     * Generated class name should be the same as the class name of the type token with {@code Impl} suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * {@code root} directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to {@code $root/java/util/ListImpl.java}.
     * Result class is not abstract and full syntax correctly.
     * Method implementations always ignore arguments and return default values for all types.
     *
     * @param token type token of interface to be implemented.
     * @param root root directory.
     * @throws ImplerException If
     *          <ul>
     *               <li>
     *                   the token as an interface cannot have a reasonable implementation
     *                   (is null, isn't interface, is interface without the possibility of implementation, etc).
     *               </li>
     *               <li>
     *                   root is uncorrected path.
     *               </li>
     *               <li>
     *                   an I/O error occurs in the process of writing text to the required file.
     *               </li>
     *          </ul>
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (token == null) {
            throw new ImplerException("Token is null.");
        }
        if (root == null) {
            throw new ImplerException("Root is null.");
        }
        if (!token.isInterface()) {
            throw new ImplerException("Token is not interface.");
        }
        if (Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Token is private.");
        }

        final Path implJavaFile = getImplJavaFile(root, token);
        createParentDir(implJavaFile);

        try (final BufferedWriter outputWriter = Files.newBufferedWriter(implJavaFile)) {
            writePackage(token.getPackageName(), outputWriter);
            writeClassSignature(token.getSimpleName() + "Impl", token.getCanonicalName(), outputWriter);

            for (Method method : token.getMethods()) {
                writeMethod(method, outputWriter);
            }

            outputWriter.write("}");
            outputWriter.newLine();
        } catch (IOException e) {
            throw new ImplerException("Error opening the output file or writing.", e);
        }
    }

    /**
     * Writes package name and empty line to {@code outputWriter}.
     * Writes according to canonical form
     *      as recommended by <cite>The Java LanguageSpecification</cite>.
     * Writes word {@code "package"} and {@code classPackage}. Doesn't write if {@code classPackage} is empty.
     *
     * @param classPackage full correct name of package.
     * @param outputWriter A {@code BufferedWriter}.
     * @throws IOException If an I/O error occurs.
     */
    private void writePackage(String classPackage, BufferedWriter outputWriter) throws IOException {
        if (classPackage.isEmpty()) {
            return;
        }
        outputWriter.write("package ");
        outputWriter.write(classPackage);
        outputWriter.write(";");
        outputWriter.newLine();
        outputWriter.newLine();
    }


    /**
     * Writes signature of class with name {@code className} and a parent interface with name {@code interfaceName}
     * to {@code outputWriter}.
     * Writes according to canonical form as recommended by <cite>The Java LanguageSpecification</cite>.
     * Always writes modificator public.
     * @param className name of class.
     * @param interfaceName full correct name of interface from which the class is inherited.
     * @param outputWriter A {@code BufferedWriter}.
     * @throws IOException If an I/O error occurs.
     */
    private void writeClassSignature(String className, String interfaceName,
                                     BufferedWriter outputWriter) throws IOException {
        outputWriter.write("public class ");
        outputWriter.write(className);
        outputWriter.write(" implements ");
        outputWriter.write(interfaceName);
        outputWriter.write(" {");
        outputWriter.newLine();
        outputWriter.newLine();
    }

    /**
     * Writes signature of {@code method} without arguments to {@code outputWriter}.
     * Writes according to canonical form
     * as recommended by <cite>The Java LanguageSpecification</cite>.
     * Writes all method modifiers and method return type and name of method.
     *
     * @param method method whose signature is to be written (must be not null).
     * @param outputWriter A {@code BufferedWriter}.
     * @throws IOException If an I/O error occurs.
     */
    private void writeMethodSignature(Method method, BufferedWriter outputWriter) throws IOException {
        outputWriter.write(TAB);
        //для transient и Varargs общий битик в getModifiers()
        outputWriter.write(Modifier.toString(method.getModifiers() & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT));
        outputWriter.write(WS);
        outputWriter.write(method.getReturnType().getCanonicalName());
        outputWriter.write(WS);
        outputWriter.write(method.getName());
    }

    /**
     * Writes primitive correct implementation of the method on the {@code returnType} to {@code outputWriter}.
     * Should do the following:
     * <ul>
     *     <li>if {@code returnType} is {@code void.class}, then do not write anything.</li>
     *     <li>if {@code returnType} is {@code boolean.class}, then write
     *     something correct and equivalent {@code "return false;"}.
     *     </li>
     *     <li>
     *         if {@code returnType} is number primitive, then do write
     *         something correct and equivalent {@code "return 0;"}.
     *     </li>
     *     <li>if {@code returnType} is a reference type, the do write {@code "return null;"}.</li>
     * </ul>
     *
     * @param returnType return type (must be not null).
     * @param outputWriter A {@code BufferedWriter}.
     * @throws IOException If an I/O error occurs.
     */
    private void writeMethodReturnResult(Class<?> returnType, BufferedWriter outputWriter) throws IOException {
        if (returnType.equals(void.class)) {
            return;
        }

        outputWriter.write(TAB.repeat(2));
        outputWriter.write("return");
        if (returnType.isPrimitive()) {
            outputWriter.write(returnType.equals(boolean.class) ? " false" : " 0");
        } else {
            outputWriter.write(" null");
        }
        outputWriter.write(";");
    }

    /**
     * Write full correct {@code method} signature and primitive implementation to {@code outputWriter}.
     * Writes according to canonical form as recommended by <cite>The Java LanguageSpecification</cite>
     * Doesn't write if the {@code method} should not be implemented in the descendant.
     * Write varargs method implementation only in vararg format, so in format {@code (Object... nameArg)}.
     *
     * @param method method whose signature is to be written (must be not null).
     * @param outputWriter A {@code BufferedWriter}.
     * @throws IOException If an I/O error occurs.
     */
    private void writeMethod(Method method, BufferedWriter outputWriter) throws IOException {
        int modifiers = method.getModifiers();
        if (Modifier.isPrivate(modifiers) || Modifier.isStatic(modifiers) || method.isDefault()) {
            return;
        }
        outputWriter.write(TAB + "@Override");
        outputWriter.newLine();

        writeMethodSignature(method, outputWriter);

        //Parameters::toString return with Generics
        //without Parameters bad with name of argument a1, a2...
        //+varargs in Parameters

        outputWriter.write(
                Arrays.stream(method.getParameters())
                        .map(p -> p.getType()
                                        .getCanonicalName()
                                        .transform(s -> p.isVarArgs() ? s.replace("[]", "...") : s)
                                + WS + p.getName()
                        )
                        .collect(Collectors.joining(", ", "(", ") {"))
        );
        outputWriter.newLine();

        writeMethodReturnResult(method.getReturnType(), outputWriter);
        outputWriter.newLine();

        outputWriter.write(TAB + "}");
        outputWriter.newLine();
        outputWriter.newLine();
    }

    /**
     * Creates a path to file with name {@code root + clazzFullName + "Impl" + type},
     * when clazzFullName is correct full name
     * of {@code clazz} taking into account the package.
     *
     * @param root root directory.
     * @param clazz type token.
     * @param type file extension.
     *          Correct operation is possible with any correct file extension, but further implementation only needs
     *          {@code .java} and {@code .class}.
     * @return the resulting path to new file.
     * @throws ImplerException if the result is not the correct path.
     */
    private Path getImplFileOnType(
            final Path root, final Class<?> clazz, final String type
    ) throws ImplerException {
        try {
            //Kgeorgiy's code
            return root.resolve(
                    (clazz.getPackageName() + "." + clazz.getSimpleName() + "Impl")
                            .replace(".", File.separator) + type).toAbsolutePath();
        } catch (InvalidPathException e) {
            throw new ImplerException("Bad path.", e);
        }
    }

    /**
     * Creates a path to file with name {@code root + clazzFullName + "Impl" + .class},
     * when clazzFullName is correct full name
     * of {@code clazz} taking into account the package.
     *
     * @param root root directory.
     * @param clazz type token.
     * @return the resulting path to new file.
     * @throws ImplerException if the result is not the correct path.
     */
    private Path getImplClassFile(final Path root, final Class<?> clazz) throws ImplerException {
        return getImplFileOnType(root, clazz, ".class");
    }

    /**
     * Creates a path to file with name {@code root + clazzFullName + "Impl" + .java},
     * when clazzFullName is correct full name
     * of {@code clazz} taking into account the package.
     *
     * @param root root directory.
     * @param clazz type token.
     * @return the resulting path to new file.
     * @throws ImplerException if the result is not the correct path.
     */
    private Path getImplJavaFile(final Path root, final Class<?> clazz) throws ImplerException {
        return getImplFileOnType(root, clazz, ".java");
    }


    /**
     * Recursively deletes the entire directory.
     * Notifies about the inability to delete (for example, when an I/O exception is caught).
     *
     * @param root root directory.
     * @see #DELETE_VISITOR
     */
    private void deleteTempDir(Path root) {
        try {
            //Kgeorgiy's code
            if (Files.exists(root)) {
                Files.walkFileTree(root, DELETE_VISITOR);
            }
        } catch (IOException e) {
            System.err.println("Error of delete TEMP_DIR");
            e.printStackTrace();
        }
    }

    /**
     * Compiles the implementation class of the passed {@code token}.
     * Produces default compilation without special options and system messages to console about warnings and errors.
     *
     * @param token type token
     *              (for correct operation, it must have an implementation with a default name
     *              for the {@link Implementor}, so suffix Impl).
     * @param root root directory.
     * @throws ImplerException If
     *          <ul>
     *              <li>Could not find java compiler on the system (have not tools.jar in classpath).</li>
     *              <li>Compilation called exit code (incorrect implementation or bad root).</li>
     *          </ul>
     */
    private void compileClass(Class<?> token, Path root) throws ImplerException {
        //by Kgeorgiy's code
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler, include tools.jar to classpath");
        }
        final String parentInterfaceClasspath;
        try {
            parentInterfaceClasspath =
                    Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (URISyntaxException e) {
            throw new ImplerException("Could not parse interfaceName to URI.", e);
        }
        final String classpath = root + File.pathSeparator + parentInterfaceClasspath;
        final String[] args = Stream.concat(
                Stream.of(getImplJavaFile(root, token).toString()),
                Stream.of("-cp", classpath, "-encoding", "UTF-8"))
                .toArray(String[]::new);
        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Compiler exit code" + exitCode + ". Error of compiling generated class.");
        }
    }

    /**
     * Produces <var>.jar</var> file implementing (only) interface specified by provided <var>token</var>.
     * <p>
     * Generated class name should be the same as the class name of the type token with <var>Impl</var> suffix
     * added.
     * Method implementations always ignore arguments and return default values for all types.
     *
     * @param token type token of interface to be implemented.
     * @param jarFile output {@code .jar} file.
     * @throws ImplerException If
     *          <ul>
     *              <li>the token as an interface cannot have a reasonable implementation
     *                  (is null, isn't interface, is interface without the possibility of implementation, etc).
 *                  </li>
     *              <li>jarFile isn't correct path.</li>
     *              <li>has compile system problems</li>
     *              <li>an I/O error occurs in the process of writing text to the required file.</li>
     *          </ul>
     *
     * @see #implement(Class, Path)
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        if (jarFile == null) {
            throw new ImplerException("Path to file.jar is null.");
        }
        createParentDir(jarFile);

        final Path tmpRoot;
        try {
            tmpRoot = Files.createTempDirectory(jarFile.getParent().toAbsolutePath(), NAME_TEMP_DIR);
        } catch (IOException | IllegalArgumentException e) {
            throw new ImplerException("Cannot create TEMP_DIR.", e);
        }

        try {
            implement(token, tmpRoot);
            compileClass(token, tmpRoot);
            try(final JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), MANIFEST)) {
                final Path implClassFile = getImplClassFile(tmpRoot, token);
                //берем относительный путь между двумя -> получаем то, что нужно
                jarOutputStream.putNextEntry(
                        new ZipEntry(
                                tmpRoot.relativize(implClassFile).toString().replace(
                                        File.separator, "/"
                                )
                            )
                        );
                Files.copy(implClassFile, jarOutputStream);
            } catch (IOException e) {
                throw new ImplerException("Write error when compressing to jar.", e);
            }
        } finally {
            deleteTempDir(tmpRoot);
        }
    }

}
