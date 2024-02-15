package info.kgeorgiy.ja.levitskii.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> elements;
    private Comparator<? super E> comparator = null;

    @SuppressWarnings("unchecked")
    private int compare(E e1, E e2) {
        if (comparator == null) {
            return ((Comparable<? super E>) e1).compareTo(e2);
        } else {
            return comparator.compare(e1, e2);
        }
    }

    public ArraySet() {
        elements = List.of();
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;
        TreeSet<E> sortedSet = new TreeSet<>(comparator);
        sortedSet.addAll(collection);
        elements = List.copyOf(sortedSet);
    }

    private ArraySet(List<E> list, Comparator<? super E> comparator) {
        this.comparator = comparator;
        this.elements = list;
    }

    @Override
    public int size() {
        return elements.size();
    }

    @Override
    public Iterator<E> iterator() {
        return elements.iterator();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    /////SUBSETS
    private int findIndElement(E e) { //O(log(n))
        int ind = Collections.binarySearch(elements, e, comparator);
        return ind >= 0 ? ind : -(ind + 1);
    }

    private ArraySet<E> getSubSetOnIndex(int left, int right) {
        return new ArraySet<>(elements.subList(left, right), comparator);
    }


    @Override
    public ArraySet<E> subSet(E fromElement, E toElement) { //O(log(n))
        if (compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("FromElement > toElement");
        }
        return getSubSetOnIndex(findIndElement(fromElement), findIndElement(toElement));
    }

    @Override
    public ArraySet<E> headSet(E toElement) {
        return getSubSetOnIndex(0, findIndElement(toElement));
    }

    @Override
    public ArraySet<E> tailSet(E fromElement) {
        return getSubSetOnIndex(findIndElement(fromElement), size());
    }

    private E getElement(int index) {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return elements.get(index);
    }

    @Override
    public E first() {
        return getElement(0);
    }

    @Override
    public E last() {
        return getElement(size() - 1);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object o) { //O(log(n)
        E eo = (E) o;
        int ind = findIndElement(eo);
        E eOnInd = ind != size() ? elements.get(ind) : null;
        return eOnInd != null && compare(eOnInd, eo) == 0;
    }
}
