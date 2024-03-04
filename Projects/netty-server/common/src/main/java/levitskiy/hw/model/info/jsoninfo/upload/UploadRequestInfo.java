package levitskiy.hw.model.info.jsoninfo.upload;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIncludeProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSetter;
import io.netty.buffer.ByteBuf;
import levitskiy.hw.model.info.jsoninfo.JSONInfo;

@JsonIncludeProperties({"pathToDir", "fileName"})
public class UploadRequestInfo implements JSONInfo {
    private String pathToDir;
    private String fileName;
    private ByteBuf byteBuf;

    public UploadRequestInfo(
            @JsonProperty("pathToDir") String pathToDir,
            @JsonProperty("fileName") String fileName) {
        this.pathToDir = pathToDir;
        this.fileName = fileName;
    }

    @JsonSetter
    public void setPathToDir(String pathToDir) {
        this.pathToDir = pathToDir;
    }

    @JsonSetter
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public void setByteBuf(ByteBuf byteBuf) {
        this.byteBuf = byteBuf;
    }

    @JsonGetter
    public String getPathToDir() {
        return pathToDir;
    }

    @JsonGetter
    public String getFileName() {
        return fileName;
    }

    public ByteBuf getByteBuf() {
        return byteBuf;
    }

    @Override
    public ByteBuf otherDataEncodeToByteBuf() {
        return byteBuf;
    }
}
