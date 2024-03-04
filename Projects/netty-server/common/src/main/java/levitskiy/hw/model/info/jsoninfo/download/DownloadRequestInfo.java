package levitskiy.hw.model.info.jsoninfo.download;

import levitskiy.hw.model.info.jsoninfo.JSONInfo;

public record DownloadRequestInfo(String pathToLoadingFile, String pathToDir) implements JSONInfo {}
