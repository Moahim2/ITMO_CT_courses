package md2html;

import markup.Markup;

public class Text implements Markup {
    protected String text;

    public  Text(String text) {
        this.text = text;
        this.text = this.text.replaceAll("&", "&amp;");
        this.text = this.text.replaceAll("<", "&lt;");
        this.text = this.text.replaceAll(">", "&gt;");

    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        sb.append(this.text);
    }

    @Override
    public void toHtml(StringBuilder sb) {
        sb.append(this.text);
    }
}
