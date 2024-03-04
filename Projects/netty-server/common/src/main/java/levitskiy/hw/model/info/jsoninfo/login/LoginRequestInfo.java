package levitskiy.hw.model.info.jsoninfo.login;

import levitskiy.hw.model.info.jsoninfo.JSONInfo;

public record LoginRequestInfo(String login, String password) implements JSONInfo {}
