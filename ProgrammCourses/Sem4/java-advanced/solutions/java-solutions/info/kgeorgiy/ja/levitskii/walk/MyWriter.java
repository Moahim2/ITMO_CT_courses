package info.kgeorgiy.ja.levitskii.walk;

import java.io.BufferedWriter;
import java.io.IOException;

public class MyWriter {
    private final BufferedWriter outputWriter;

    MyWriter(BufferedWriter outputWriter) {
        this.outputWriter = outputWriter;
    }

    public void writeResult(String hex, String name) throws IOException {
        outputWriter.write(hex + " " + name);
        outputWriter.newLine();
    }

    public void writeHexCodeError(String name) throws IOException {
        writeResult("0".repeat(64), name);
    }
}
