package md2html;

import markup.AbstractMarkup;
import markup.Markup;

import java.util.List;

public class Quote extends AbstractMarkup {
    public Quote(List<Markup> markups) {
        super(markups);
        typeHtml = "q";
    }
}
