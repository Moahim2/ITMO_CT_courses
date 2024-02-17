package levitskiy.hw;

import java.io.IOException;

public class KotlinCompiler {

    public static boolean compile(String name) {
        try {
            int exitValue = new ProcessBuilder("cmd.exe")
                    .command(
                            "cmd",
                            "/c",
                            "kotlinc " + name + " -include-runtime -d " + name.replace(".kt", ".jar"))
                    .start()
                    .waitFor();
            return exitValue == 0;
        } catch (IOException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
