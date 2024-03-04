package levitskiy.hw.model.info.jsoninfo.change;

import levitskiy.hw.model.info.jsoninfo.JSONInfo;

public record ChangeDirRequestInfo(String oldFilePathName, String newDirName) implements JSONInfo {}
