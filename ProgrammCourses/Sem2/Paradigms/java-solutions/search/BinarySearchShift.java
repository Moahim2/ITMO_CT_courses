package search;

public class BinarySearchShift {
    //Контракт:
    //Pred: существует единственное k из (0...n - 1): for i:1...k - 1, for j:k...n - 1 a[i] > a[i - 1], a[i] > a[j], a[j] > a[j - 1] && a[k - 1 % n] == maximum
    //Post R == k
    static int iterateSearchShift(int[] arr) {
        //pred -> a[n - 1] == max(a[k]...a[n-1])
        // n >= k && -1 < k
        int r = arr.length, l = -1;
        //I: r >= k && l < k
        while (r > l + 1) {
            // r >= k && l < k && r > l + 1 ->
            // -> r >= k && l < k && r > l + 1 && l < (l + r)/2 < r
            int m = (l + r) / 2;
            // r >= k && l < k &&  l < m < r
            if (arr[m] > arr[arr.length - 1]) {
                //r >= k && l < k  && l < m < r && a[m] > a[n - 1]
                //r >= k && (т.к. a[m] > max(a[k]...a[n - 1]) && a[k] <... < a[n - 1] -> m < k)
                l = m;
                // r >= k && l < k
            } else {
                //r >= k && l < k  && l < m < r && a[m] <= a[n - 1]
                // l < k && (т.к. a[m] <= a[n - 1] && a[0]...a[k - 1] > a[n - 1] -> r >= k
                r = m;
                // r >= k && l < k
            }
            //Q == I
        }
        //Цикл конечен: l < m < r && (l' == m || r' == m) ->
        //-> т.к. (r - l) - конечное и (r' - l') < (r - l) -> цикл конечен

        // r >= k && l < k && r <= l + 1 ->
        //-> l + 1 <= k -> r <= l + 1 <= k -> r >= k && r <= k -> r == k
        return r;
        // R == k
    }



    //Контракт:
    //Pred: существует единственное k из (0...n - 1): for i:1...k - 1, for j:k...n-1 a[i] > a[i - 1], a[i] > a[j], a[j] > a[j - 1] && a[k - 1 % n] == maximum &&
    //&& r >= k && l < k (выполнено при вызове)
    //Post R == k
    static int recursiveSearchShift(int[] arr, int l, int r) {
        //pred -> a[n - 1] == max(a[k]...a[n-1])

        //r >= k && l < k
        if (r <= l + 1) {
            // r >= k && l < k && r <= l + 1 ->
            //-> l + 1 <= k -> r <= l + 1 <= k -> r >= k && r <= k -> r == k
            return r;
            // R == k
        } else {
            // r >= k && l < k && r > l + 1 ->
            // -> r >= k && l < k && r > l + 1 && l < (l + r)/2 < r
            int m = (l + r) / 2;
            // r >= k && l < k &&  l < m < r
            if (arr[m] > arr[arr.length - 1]) {
                //r >= k && l < k  && l < m < r && a[m] > a[n - 1]
                //r >= k && (т.к. a[m] > max(a[k]...a[n - 1]) && a[k] <...< a[n - 1] -> m < k)
                //pred && r >= k && m < k -> Pred - выполняется
                return recursiveSearchShift(arr, m, r);
                //R == k
            } else {
                //r >= k && l < k  && l < m < r && a[m] <= a[n - 1]
                // l < k && (т.к. a[m] <= a[n - 1] && a[0]...a[k - 1] > a[n - 1] -> r >= k
                //pred && m >= k && l < k
                return recursiveSearchShift(arr, l, m);
                //R == k
            }
        }
        //Рекурсия конечна: l < m < r && (l' == m || r' == m) ->
        //-> т.к. (r - l) - конечное и (r' - l') < (r - l) -> рекурсия конечна
    }






    /* Примечания (контракты для всех ф-ций в main):
          Контракт Integer.parseInt(a):
              Pred: a - целое число правильное записанное в формате строки
              Post: R == a (переведенному в число)
          Контракт System.out.println(a):
              Pred: a - объект приводящийся к строке (в нашем случае достаточно целого числа)
              Post: R - вывод числа в консоль
          Контракты iterateBinarySearch и recursiveBinariSearch для их использования в main уже прописаны выше
     */




    //Контракт: k
    /*Pred: args[i] - целые числа правильно записанные в формате строк &&
           (если перевести строки в числа) существует единственное k из (0...n - 1): for i:1...k - 1,
           for j:k...n - 1 a[i] > a[i - 1], a[i] > a[j], a[j] > a[j - 1] && a[k - 1 % n] == maximum    */
    //Post: в консоль выводится R == k
    public static void main(String[] args) {

        int[] arr = new int[args.length];

        int i = 0;
        while (i < args.length) {
            arr[i] = Integer.parseInt(args[i]);
            i++; // i = i + 1
        }
        int ans1 = iterateSearchShift(arr);
        //int ans2 = recursiveSearchShift(arr, -1, arr.length);
        //assert ans1 == ans2; //проверка одновременно обеих ф-ций
        System.out.println(ans1);
    }
}
