package hw.levitskiy.parser;

import java.util.List;
import java.util.Optional;

public record RuleRightPart(List<String> trans, Optional<String> returnCode, List<String> listArgCode) {}
