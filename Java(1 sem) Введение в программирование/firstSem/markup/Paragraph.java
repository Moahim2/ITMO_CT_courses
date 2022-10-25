package markup;

import java.util.List;

public class Paragraph extends AbstractMarkup{

    public Paragraph(List<Markup> markups) {
        super(markups);
        typeMarkup = "";
        typeHtml = "";
    }

}
