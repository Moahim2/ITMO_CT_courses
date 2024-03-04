package levitskiy.hw.converter;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.MessageToByteEncoder;
import levitskiy.hw.model.PackedData;

import java.nio.charset.Charset;

/**
 * Supports coding protocol for {@link PackedData}: int + string + int + string + int + bytes,
 * where int is the number of bytes to encode the next token.
 * @see PackedDataDecoder
 */
public class PackedDataEncoder extends MessageToByteEncoder<PackedData> {
    private final Charset charset;

    public PackedDataEncoder(Charset charset) {
        this.charset = charset;
    }


    @Override
    protected void encode(ChannelHandlerContext ctx, PackedData data, ByteBuf out) {
        encodeString(data.typeCommand(), out, charset);
        encodeString(data.stringArguments(), out, charset);
        if (data.otherData() != null) {
            out.writeInt(data.otherData().readableBytes());
            out.writeBytes(data.otherData());
        } else {
            out.writeInt(0);
        }
    }

    private static void encodeString(String str, ByteBuf out, Charset charset) {
        byte[] strBytes = str.getBytes(charset);
        out.writeInt(strBytes.length);
        out.writeBytes(strBytes);
    }
}
