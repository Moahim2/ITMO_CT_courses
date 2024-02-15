package hw.hw4;


import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class MultipartExampleClient {
    private static final String USAGE = "usage:\n\tjava "
            + MultipartExampleClient.class.getSimpleName()
            + " <address> [<field name>:<field text> OR <field name>=<file name> OR <field name>%<json>]...\n"
            + "example:\n\tjava "
            + MultipartExampleClient.class.getSimpleName()
            + " http://example.com/ name=Me avatar:selfie.jpg\n";

    public void mainF(String[] args) throws IOException {
        switch (args.length) {
            case 0:
            case 1:
                System.err.println(USAGE);
                break;
            default:
                final List<Field> fields = new ArrayList(args.length - 1);
                for (int i = 1; i < args.length; ++i) {
                    final String arg = args[i];
                    final int ei = arg.indexOf('=');
                    final int pi = arg.indexOf('%');
                    final int si = arg.indexOf(':');
                    if (ei != -1 && (ei < pi || pi == -1) && (ei < si || si == -1)) {
                        fields.add(new Field(arg.substring(0, ei), arg.substring(ei + 1), Field.Type.FILE));
                    } else if (pi != -1 && (pi < si || si == -1)) {
                        fields.add(new Field(arg.substring(0, pi), arg.substring(pi + 1), Field.Type.JSON));
                    } else if (si != -1) {
                        fields.add(new Field(arg.substring(0, si), arg.substring(si + 1), Field.Type.TEXT));
                    } else {
                        System.err.println("What is " + arg + "?");
                        System.err.println(USAGE);
                        return;
                    }
                }
                final URL url = new URL(args[0]);
                send(url, fields);
        }
    }

    private static void send(final URL url, final List<Field> fields) throws IOException {
        final HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setDoOutput(true);
        connection.setDoInput(true);
        connection.setRequestMethod("POST");
        final String boundary = MultipartTool.generateBoundary();
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
        final OutputStream outputStream = connection.getOutputStream();
        final MultipartTool tool = new MultipartTool(outputStream, boundary);
        try {
            for (final Field f : fields) {
                switch (f.type) {
                    case TEXT:
                        tool.appendField(f.name, f.data, "text/plain; charset=utf-8");
                        break;
                    case FILE:
                        tool.appendFile(f.name, new File(f.data));
                        break;
                    case JSON:
                        tool.appendJsonField(f.name, f.data);
                        break;
                    default:
                        throw new IllegalStateException(f.type.name() + " is unknown");
                }
            }
        } finally {
            tool.close();
            outputStream.close();
        }
        final int code = connection.getResponseCode();
        final PrintStream out = (200 <= code && code < 300) ? System.out : System.err;
        out.println(connection.getResponseMessage());
        try (BufferedReader resp = new BufferedReader(new InputStreamReader(connection.getInputStream()))) {
            //noinspection StatementWithEmptyBody
            for (String line; (line = resp.readLine()) != null; out.println(line)) ;
        }
        connection.disconnect();
    }

    private static class Field {
        final String name;
        final String data;
        final Type type;

        private Field(String name, String data, Type type) {
            this.name = name;
            this.data = data;
            this.type = type;
        }

        enum Type {
            TEXT, FILE, JSON
        }
    }
}
