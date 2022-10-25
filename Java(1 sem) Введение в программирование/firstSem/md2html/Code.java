package md2html;

import markup.AbstractMarkup;
import markup.Markup;

import java.util.List;

public class Code extends AbstractMarkup {

    public Code(List<Markup> markups) {
        super(markups);
        typeHtml = "code";
    }
}
