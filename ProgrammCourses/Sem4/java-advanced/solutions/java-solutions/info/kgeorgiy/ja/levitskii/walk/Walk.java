package info.kgeorgiy.ja.levitskii.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Walk {
    private static MessageDigest md;
    private static MyWriter myWriter;

    public static void main(String[] args) {
        if (args == null || args.length != 2) {
            System.err.println("Wrong number of arguments");
            return;
        }
        if (args[0] == null || args[1] == null) {
            System.err.println("Argument is null");
            return;
        }
        Path outFilePath;
        Path inputFilePath;
        try {
            inputFilePath = Paths.get(args[0]);
            outFilePath = Paths.get(args[1]);
        } catch (InvalidPathException e) { //Paths.get -> throws
            System.err.println("Bad startPath arguments");
            return;
        }

        Path outFileParentPath = outFilePath.getParent();
        if (outFileParentPath != null && !Files.exists(outFileParentPath)) {
            try {
                Files.createDirectory(outFileParentPath);
            } catch (IOException | SecurityException e) {
                System.err.println("Failed to create parent directory for output file");
                return;
            }
        }
        try {
            md = MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException ignored) {
            //Кажется такой случай невозможен, т.к. SHA-256 обязано быть в java
            return;
        }

        try (BufferedWriter out = Files.newBufferedWriter(outFilePath)) {
            try (BufferedReader in = Files.newBufferedReader(inputFilePath)) {
                myWriter = new MyWriter(out);
                String curLine;
                while ((curLine = in.readLine()) != null) {
                    try {
                        try {
                            visitFile(Paths.get(curLine));
                        } catch (SecurityException | IllegalArgumentException e) {
                            myWriter.writeHexCodeError(curLine);
                        }
                    } catch (IOException e) {
                        System.err.println("Error writing to the output file");
                    }
                }
            } catch (IOException e) {
                // :NOTE: messages
                System.err.println("Error reading/opening the input file");
            } catch (SecurityException e) {
                System.err.println("Error opening the input file");
            }
        } catch (IOException | SecurityException e) {
            System.err.println("Error opening the output file");
        }
    }

    private static void visitFile(Path file) throws IOException {
        try (DigestInputStream ds = new DigestInputStream(Files.newInputStream(file), md)) {
            while (ds.readNBytes(8192).length != 0) {
                md = ds.getMessageDigest();
            }
        } catch (IOException e) {
            myWriter.writeHexCodeError(file.toString());
            return;
        }
        StringBuilder hexSHA = new StringBuilder();
        for (byte i : md.digest()) { //byte to HexDecimal
            hexSHA.append(String.format("%02x", i));
        }
        myWriter.writeResult(hexSHA.toString(), file.toString());
    }
}
