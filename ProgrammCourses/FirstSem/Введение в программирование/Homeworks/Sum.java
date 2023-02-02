public class Sum {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 0; i < args.length; i++) {
            String num = args[i];
            for (int begin = 0; begin < num.length(); begin++) {
                if (Character.isWhitespace(num.charAt(begin)) != true) {
                    for (int end = begin; end < num.length(); end++) {
                        if (Character.isWhitespace(num.charAt(end))) {
                            sum += Integer.parseInt(num.substring(begin, end));
                            begin = end;
                            break;
                        }
                        if (end == num.length() - 1) {
                            sum += Integer.parseInt(num.substring(begin, end + 1));
                            begin = end;
                            break;
                        }
                    }
                }
            }
        }
    System.out.println(sum);
    }
}