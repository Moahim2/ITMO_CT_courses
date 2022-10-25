package markup;

import java.util.List;

public class Main {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder();
        Paragraph paragraph = new Paragraph(List.of(
                new Strong(List.of(
                        new Text("abcdef"),
                        new Strikeout(List.of(
                                new Text("ghy"),
                                new Emphasis(List.of(
                                        new Text("3456"),
                                        new Text("4")
                                )),
                                new Text("5")
                        )),
                        new Text("6")
                ))
        ));
        paragraph.toHtml(sb);
        System.out.println(sb);
    }
}


