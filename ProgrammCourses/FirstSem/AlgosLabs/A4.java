import java.io.IOException;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.*;


public class A4 {
    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(
            new InputStreamReader(
                new FileInputStream("muggers.in"),
                "utf-8"
            )
        );
        String a = in.readLine();
        String[] aa = a.split(" ");
        int n = Integer.parseInt(aa[0]);
        int k = Integer.parseInt(aa[1]);
        int[] inp = new int[n + 1];
        inp[0] = 0;
        String s = in.readLine();
        String[] ss = s.split(" ");
        int[] answer = new int[n + 1];
        answer[0] = 0;

        for (int i = 1; i < n + 1; i++) {
            inp[i] = Integer.parseInt(ss[i - 1]);
        }

        for (int i = 1; i < n + 1; i++) {
            int min = -1;
            for (int j = i - 1; j >= i - k - 1 && j >= 0; j--) {
                if (min == -1) {
                    min = answer[j];
                } else if (min > answer[j]) {
                    min = answer[j];
                }
            }
            answer[i] = min + inp[i];
        }
        in.close();
        BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(
                        new FileOutputStream("muggers.out"),
                        "utf-8"
                    )
                );

        out.write(Integer.toString(answer[n]));
        out.close();
    }
}