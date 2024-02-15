package search;

public class BinarySearch {
    /* Условимся в дальнейших рассуждениях, что если мы рассматриваем в выражении значение arr[i] и i вылезает за
        границы массива, то arr[i] = +inf если i < 0 и arr[i] = -inf если i >= arr.length. Это позволяет без лишних рассуждений
            обрабатывать крайние случаи.
        Также если постусловие совпадает с предусловием следующего то комментарий не дублируется
        Также для краткости не дублирую Post-условие (его часть) из Контракта для функции бинпоиска "о том, что массив отсортирован по невозрастанию"
            т.к. оно выполняется независимо после каждой операции (я им просто воспользуюсь в нужный момент)
     */

    //Контракт:
        //Pred: for i=0...n-1: arr[i] >= arr[i + 1], т.е. массив отсортирован по невозрастанию
        //Post: R, такой что for i < R: arr[i] > x && for i >= R: arr[i] <= x
    public static int iterateBinarySearch(int x, int[] arr) {
        // for r' >= arr.length: arr[r'] = -inf <= x && for l' <= l: arr[l'] = +inf > x
        int r = arr.length, l = -1;
        // I: for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x
        while (r > l + 1) {
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 ->
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < (l + r)/2 < r
                    //(последний коньюнкт следует из предпоследнего напрямую)
            int m = (l + r) / 2;
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r
            if (arr[m] > x) {
                //arr[m] > x && for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r ->
                //-> for r' >= r: arr[r'] <= x && for l' <= m: arr[l'] > x (в переходе воспользовались тем что массив невозрастающий)
                l = m;
                //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && l == m
            } else {
                //arr[m] <= x && for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r ->
                //-> for r' >= m: arr[r'] <= x && for l' <= l: arr[l'] > x (в переходе воспользовались тем что массив невозрастающий)
                r = m;
                //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r == m
            }
            //Q == I: for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x (оба пути ветвления дают этот инвариант)

        }
        //Цикл конечен: l < m < r && (l' == m || r' == m) ->
        //-> т.к. (r - l) - конечное и (r' - l') < (r - l) -> цикл конечен

        //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r <= l + 1 <->
        // <-> for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r - 1 <= l ->
        //-> for r' >= r: arr[r'] <= x && for l' <= r - 1 <= l: arr[l'] > x ->
        //-> for i >= r: arr[i] <= x && for i < r: arr[i] > x
        //-> for i >= R: arr[i] <= x && for i < R: arr[i] > x - что и требовалось доказать
        return r;
        //r == R
    }




    //Контракт:
    /*Pred: for i=1...n-1: arr[i] >= arr[i + 1], т.е. массив отсортирован по невозрастанию &&
            for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x    */
    //Post: R, такой что for i < R: arr[i] > x && for i >= R: arr[i] <= x
    static int recursiveBinariSearch(int x, int[] arr, int l, int r) {
        //I == P == Pred (не меняется и совпадает с общим предусловием)
        if (r <= l + 1) {
            //r <= l + 1 && for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x <->
            // <-> for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r - 1 <= l ->
            // -> for r' >= r: arr[r'] <= x && for l' <= r - 1 <= l: arr[l'] > x ->
            // -> for i >= r: arr[i] <= x && for i < r: arr[i] > x
            // -> for i >= R: arr[i] <= x && for i < R: arr[i] > x - что и требовалось доказать
            return r;
            // R == r
        } else {
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 ->
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < (l + r)/2 < r
            //(последний коньюнкт следует из предпоследнего напрямую)
            int m = (l + r) / 2;
            //for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r
            if (arr[m] > x) {
                //arr[m] > x && for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r ->
                //-> for r' >= r: arr[r'] <= x && for l' <= m: arr[l'] > x (в переходе воспользовались тем что массив невозрастающий)
                //I: на этом моменте доказали что предусловие после входа в рекурсию перед рекурсивным вызовом совпадает с изначальным предусловием
                return recursiveBinariSearch(x, arr, m, r);
                //Q == Post (требуем по контракту) && R' == R (в силу конечности рекурсии)
            } else {
                //arr[m] <= x && for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x && r > l + 1 && l < m < r ->
                //-> for r' >= m: arr[r'] <= x && for l' <= l: arr[l'] > x (в переходе воспользовались тем что массив невозрастающий)
                //I: на этом моменте доказали что предусловие после входа в рекурсию перед рекурсивным вызовом совпадает с изначальным предусловием
                return recursiveBinariSearch(x, arr, l, m);
                //Q == Post (требуем по контракту) && R' == R (в силу конечности рекурсии)
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


    /*Pred: args[i] - целые числа правильно записанные в формате строк &&
           (если перевести строки в числа) for i:(args.length - 1) > i > 0, args[i] >= args[i + 1]   */
    /*Post: в консоль выводится наименьшее i, такое что args[i - 1] <= args[0] || (если нет такого i) то выводится args.length - 1 <->
        <-> R, такой что для любого i < R: arr[i] > x && для любого i >= R: arr[i] <= x
     */
    public static void main(String[] args) {
        int x = Integer.parseInt(args[0]);

        int[] arr = new int[args.length - 1];

        int i = 1;
        while (i < args.length) {
            arr[i - 1] = Integer.parseInt(args[i]);
            i++; // i = i + 1
        }

        //P1: for i:0...n-1: arr[i] >= arr[i + 1]
        int ans1 = iterateBinarySearch(x, arr);
        //Q1 ans1 == R1 && R1: for i < R1: arr[i] > x && for i >= R1: arr[i] <= x
        /*P2:for i: arr[i] >= arr[i + 1], т.е. массив отсортирован по невозрастанию &&
                   for r' >= r: arr[r'] <= x && for l' <= l: arr[l'] > x (Это истина, т.к. при l = -1
                   && r = arr.length условия очевидно выполняются)*/
        // int ans2 = recursiveBinariSearch(x, arr, -1, arr.length);
        //Q2: ans2 == R2 && R2, такой что for i < R2: arr[i] > x && for i >= R2: arr[i] <= x
        //P: R1 == R2 из их определений (по Q1 и Q2)
        //assert ans1 == ans2; //просто проверка что сразу оба поиска правильные
        System.out.println(ans1);
        /* в консоль выводится наименьшее i, такое что args[i - 1] <= args[0] || (если нет такого i) то выводится args.length - 1 <->
        <-> R, такой что для любого i < R: arr[i] > x && для любого i >= R: arr[i] <= x */
    }
}
