package markup;

import java.util.List;

public class Strong extends AbstractMarkup {

    public Strong(List<Markup> markups) {
        super(markups);
        typeMarkup = "__";
        typeHtml = "strong";
    }

}
