package queue;

import java.util.function.Predicate;

//Model: a[1]...a[n]; a[1] - last, a[n] - first
//I: for 1...n: a[i] != null
public interface Queue {
    //Immutable(n): for i=1...n: a'[i]==a[i]

    //Контракт:
    //Pred: element != null
    //Post: n' == n + 1 && a[1] = element && for i=1...n: a'[i + 1] = a[i]
    void enqueue(Object element);

    //Контракт:
    //Pred: n >= 1
    //Post: R = a[n] && n' == n && immutable(n)
    Object element();

    //Контракт:
    //Pred: n >= 1
    //Post: R == a[n] && n' == n - 1 && immutable(n')
    Object dequeue();

    //Контракт:
    //Pred: true
    //Post: R == n && n' == n && immutable(n)
    int size();

    //Контракт:
    //Pred: true
    //Post: R == (n > 0) && n' == n && immutable(n)
    boolean isEmpty();

    //Контракт:
    //Pred: true
    //Post: n == 0
    void clear();

    //Контракт
    //Pred:predicate != null
    //Post:R = n - x: x = max(i_1,...i_k): test(a[i_j]) == true || R == -1 for i=1...n test(a[i]) == false;
    int indexIf(Predicate<Object> predicate);

    //Контракт
    //Pred:predicate != null
    //Post:R = n - x: x = min(i_1,...i_k): test(a[i_j]) == true || R == -1 for i= 1...n test(a[i]) == false;
    int lastIndexIf(Predicate<Object> predicate);
}
