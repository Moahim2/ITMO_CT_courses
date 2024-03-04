package levitskiy.hw.converter;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ReplayingDecoder;
import levitskiy.hw.model.PackedData;

import java.nio.charset.Charset;
import java.util.List;

/**
 * Supports coding protocol for {@link PackedData}: int + string + int + string + int + bytes,
 * where int is the number of bytes to encode the next token.
 * @see PackedDataEncoder
 */
public class PackedDataDecoder extends ReplayingDecoder<PackedData> {
    private final Charset charset;

    public PackedDataDecoder(Charset charset) {
        this.charset = charset;
    }

    @Override
    protected void decode(ChannelHandlerContext ctx, ByteBuf in, List<Object> out) {
        String commandType = readString(in, charset);
        String stringsArguments = readString(in, charset);
        int countBytes = in.readInt();
        ByteBuf otherData = countBytes > 0 ? in.readBytes(countBytes) : null;

        out.add(new PackedData(commandType, stringsArguments, otherData));
    }

    private static String readString(ByteBuf in, Charset charset) {
        int len = in.readInt();
        return in.readCharSequence(len, charset).toString();
    }
}
