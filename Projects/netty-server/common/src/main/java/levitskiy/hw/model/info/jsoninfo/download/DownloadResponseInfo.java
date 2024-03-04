package levitskiy.hw.model.info.jsoninfo.download;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIncludeProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSetter;
import io.netty.buffer.ByteBuf;
import levitskiy.hw.model.info.jsoninfo.JSONInfo;

@JsonIncludeProperties({"message", "pathToClientDir", "fileName"})
public class DownloadResponseInfo implements JSONInfo {
    private String message;
    private String pathToClientDir;
    private String fileName;
    private ByteBuf byteBuf;

    public DownloadResponseInfo(
            @JsonProperty("message") String message,
            @JsonProperty("pathToClientDir") String pathToClientDir,
            @JsonProperty("fileName") String fileName
            ) {
        this.message = message;
        this.pathToClientDir = pathToClientDir;
        this.fileName = fileName;
    }

    @Override
    public ByteBuf otherDataEncodeToByteBuf() {
        return byteBuf;
    }

    @JsonGetter
    public String getMessage() {
        return message;
    }

    @JsonGetter
    public String getPathToClientDir() {
        return pathToClientDir;
    }

    @JsonGetter
    public String getFileName() {
        return fileName;
    }

    public ByteBuf getByteBuf() {
        return byteBuf;
    }

    @JsonSetter
    public void setMessage(String message) {
        this.message = message;
    }

    @JsonSetter
    public void setPathToClientDir(String pathToClientDir) {
        this.pathToClientDir = pathToClientDir;
    }

    @JsonSetter
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public void setByteBuf(ByteBuf byteBuf) {
        this.byteBuf = byteBuf;
    }
}
