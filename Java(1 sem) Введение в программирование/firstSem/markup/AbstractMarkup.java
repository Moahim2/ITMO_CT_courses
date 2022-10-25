package markup;

import java.util.List;

public abstract class AbstractMarkup implements Markup {
    protected List<Markup> markups;
    protected String typeMarkup;
    protected String typeHtml;
    public AbstractMarkup(List<Markup> markups) {
        this.markups = markups;
    }

    public void toMarkdown(StringBuilder sb) {
        sb.append(typeMarkup);
        for (Markup markup : markups) {
            markup.toMarkdown(sb);
        }
        sb.append(typeMarkup);
    }
    public void toHtml(StringBuilder sb) {
        if (!typeHtml.equals("")) {
            sb.append("<" + typeHtml + ">");
        }
        for (Markup markup : markups) {
            markup.toHtml(sb);
        }
        if (!typeHtml.equals("")) {
            sb.append("<" + "/" + typeHtml + ">");
        }
    }
}
