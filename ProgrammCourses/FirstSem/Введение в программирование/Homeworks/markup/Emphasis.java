package markup;
import java.util.List;

public class Emphasis extends AbstractMarkup {

    public Emphasis(List<Markup> markups) {
        super(markups);
        typeMarkup = "*";
        typeHtml = "em";
    }

}
