package info.kgeorgiy.ja.levitskii.crawler;

import info.kgeorgiy.java.advanced.crawler.CachingDownloader;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

public class WCMain {
    public static void main(String[] args) throws IOException, InterruptedException {
        WebCrawler webCrawler = new WebCrawler(new CachingDownloader(1), 10, 10, 1);
            var t1 = new Thread(() -> {
                webCrawler.download("https://e-maxx.ru/algo/assignment_hungary", 2);
            });
            var t2 = new Thread(() -> {
                webCrawler.download("https://alphapedia.ru/w/Edmonds–Karp_algorithm", 1);
            });
            var t3 = new Thread(() -> {
                webCrawler.download("https://ru.wikipedia.org/wiki/Чёрная_дыра", 1);
            });
            t1.start();
            t2.start();
            t3.start();
            t1.join();
            t2.join();
            t3.join();
            webCrawler.close();
    }

}
