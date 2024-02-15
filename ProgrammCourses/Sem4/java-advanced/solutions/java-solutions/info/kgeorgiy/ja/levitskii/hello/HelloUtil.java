package info.kgeorgiy.ja.levitskii.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;

/**
 * The auxiliary class for rendering common client and server methods.
 *
 * @author Ivanl
 */
public class HelloUtil {

    /**
     * Special char {@value} for creating request.
     */
    public static final char REQUEST_SPECIAL_CHAR = '_';


    public static final Charset CHARSET = StandardCharsets.UTF_8;

    /**
     * The equivalent of processing a string on the server.
     *
     * @param s string.
     * @return {@code Hello, s}.
     */
    public static String createResponse(String s) {
        return  "Hello, " + s;
    }



    /**
     * Checks if the server response is correct.
     *
     * @param response answer of server.
     * @param requestStr request.
     *
     * @return true if {@code response} - correct answer.
     */
    public static boolean isCorrectResponse(String response, String requestStr) {
        return response != null && response.contains(requestStr);
    }


    /**
     * Returns the string stored in the package.
     *
     * @param packet datagram packet of udp.
     * @return data of packet in string format.
     */
    public static String getStringFromDatagramPacket(DatagramPacket packet) {
        return new String(
                packet.getData(),
                packet.getOffset(),
                packet.getLength()
        ); // :NOTE: CHARSET?
    }


    /**
     * Creates new buffer with valid buffer size on this {@code socket}.
     *
     * @param socket udp socket.
     *
     * @return full valid buffer on this {@code socket}
     * @throws SocketException if there is an error in the underlying protocol, such as an UDP error.
     */
    public static byte[] getValidReceiveBuffer(DatagramSocket socket) throws SocketException {
        //by kgeorgiy
        return new byte[socket.getReceiveBufferSize()];
    }


    /**
     * Creates datagram packet for UDP receiving.
     *
     * @param buf buffer (expected valid buffer after {@link DatagramSocket#getReceiveBufferSize()}).
     * @return new DatagramPacket with buffer {@code buf} and length equals {@code buf.size()}.
     */
    public static DatagramPacket createPacket(byte[] buf) {
        return new DatagramPacket(buf, buf.length);
    }


    /**
     * Equivalent to calling the {@link DatagramSocket#send(DatagramPacket packet)}
     * method on {@code socket} when packet is {@code DatagramPacket(data, data.length, address)}.
     *
     * @param socket udp socket.
     * @param data data.
     * @param address address.
     * @throws IOException if an I/O error occurs.
     */
    public static void sendData(DatagramSocket socket, byte[] data, SocketAddress address) throws IOException {
        socket.send(new DatagramPacket(data, data.length, address));
    }



    /**
     * Checks the entered data for correctness (size and not contains nulls).
     *
     * @param args (usually cmd) arguments.
     * @param expectedSize expected size of {@code args}.
     *
     * @return true if args is correct array of arguments with correct size.
     */
    public static boolean checkArgs(String[] args, int expectedSize) {
        if (args == null) {
            System.err.println("Array of args is null.");
            return true;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Array of args contains null");
            return true;
        }

        if (args.length != expectedSize) {
            System.err.println("Array of args of unacceptable size (must be " + expectedSize + " arguments)");
            return true;
        }
        return false;
    }

}
