/*
 * DO NOT FORGET TO PUT package ...; HERE!
 * DO _NOT_ USE com.example.*!
 *
 * package io.github.[MyName].android.hw4;
 */

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URLConnection;

/**
 * Helper for {@code multipart/form-data} requests
 */
public class MultipartTool implements Closeable {
    private static final String CRLF = "\r\n";

    /**
     * {@link System#currentTimeMillis()}-based unique char sequence generator
     * @return some sequence
     */
    public static String generateBoundary() {
        return "------" + System.currentTimeMillis() + "------";
    }

    private final OutputStream connectionOS;
    private final Writer connectionW;
    private final String boundary;

    /**
     * Wrapper for {@link OutputStream} from {@link HttpURLConnection}
     * which allows easily send {@code multipart/form-data} requests.
     * <p>
     * <b>NOTE:</b> any other {@link HttpURLConnection} setup must be done
     * before opening connection (via e.g. getting output stream) for sending
     * multipart requests. This class <i>does not</i> perform such actions.
     *
     * @param connectionOS output stream from {@link HttpURLConnection#getOutputStream()}
     * @param boundary some unique data, must be put in {@code Content-Type} header
     *                 like this:
     *                 <pre>conn.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);</pre>
     * @see #generateBoundary()
     */
    public MultipartTool(OutputStream connectionOS, String boundary) {
        this.connectionOS = connectionOS;
        this.boundary = boundary;
        this.connectionW = new OutputStreamWriter(connectionOS);
    }

    public void appendJsonField(final String fieldName, final String data) throws IOException {
        appendField(fieldName, data, "application/json; charset=utf-8");
    }

    public void appendField(final String fieldName,
                            final String data,
                            final String contentType) throws IOException {
        connectionW.append("--").append(boundary).append(CRLF)
                .append("Content-Disposition: form-data; name=\"").append(fieldName).append('"').append(CRLF)
                .append("Content-Type: ").append(contentType).append(CRLF)
                .append(CRLF)
                .append(data).append(CRLF)
                .flush();
    }

    public void appendFile(final String fieldName,
                           final byte[] data,
                           final String contentType,
                           final String fileName) throws IOException {
        appendFile(fieldName, data, 0, data.length, contentType, fileName);
    }

    public void appendFile(final String fieldName,
                           final byte[] data,
                           final int offset,
                           final int len,
                           final String contentType,
                           final String fileName) throws IOException {
        appendFilePreamble(fieldName, contentType, len - offset, fileName);
        connectionOS.write(data, offset, len);
        appendFilePS();
    }

    public void appendFile(final String fieldName,
                           final File file) throws IOException {
        appendFile(fieldName, file, URLConnection.guessContentTypeFromName(file.getName()));
    }

    public void appendFile(final String fieldName,
                           final File file,
                           final String contentType) throws IOException {
        final FileInputStream fis = new FileInputStream(file);
        try {
            appendFile(fieldName, fis, contentType, file.length(), file.getAbsolutePath());
        } finally {
            fis.close();
        }
    }

    /**
     * Appends file provided via {@link InputStream}. It is caller responsibility
     * to close that stream.
     */
    public void appendFile(final String fieldName,
                           final InputStream data,
                           final String contentType,
                           final long contentLength,
                           final String fileName) throws IOException {
        appendFilePreamble(fieldName, contentType, contentLength, fileName);
        final byte[] buffer = new byte[4096];
        //noinspection StatementWithEmptyBody
        for (int n; (n = data.read(buffer)) != -1; connectionOS.write(buffer, 0, n));
        appendFilePS();
    }

    @Override
    public void close() throws IOException {
        connectionW.append(CRLF)
                .append("--").append(boundary).append("--")
                .append(CRLF)
                .close();
    }

    private void appendFilePreamble(String fieldName, String contentType, long contentLength, String fileName) throws IOException {
        connectionW.append("--").append(boundary).append(CRLF)
                .append("Content-Disposition: form-data; name=\"").append(fieldName)
                .append("\"; filename=\"").append(fileName).append('"').append(CRLF)
                .append("Content-Type: ").append(contentType).append(CRLF)
                .append("Content-Length: ").append(Long.toString(contentLength)).append(CRLF)
                .append("Content-Transfer-Encoding: binary").append(CRLF)
                .append(CRLF)
                .flush();
    }

    private void appendFilePS() throws IOException {
        connectionOS.flush();
        connectionW.append(CRLF).flush();
    }
}
