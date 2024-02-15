package info.kgeorgiy.ja.levitskii.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements StudentQuery {
    private <T> Stream<T> getFieldStream(Function<Student, T> getStudentField, List<Student> students) {
        return students.stream().map(getStudentField);
    }

    private <T> List<T> getFieldList(Function<Student, T> getStudentField, List<Student> students) {
        return getFieldStream(getStudentField, students).toList();
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return getFieldList(Student::getFirstName, students);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return getFieldList(Student::getLastName, students);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return getFieldList(Student::getGroup, students);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return getFieldList(student -> student.getFirstName() + " " + student.getLastName(), students);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return getFieldStream(Student::getFirstName, students).collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName)
                .orElse("");
    }

/////////

    private List<Student> sortedListStudentsByComparator(
            Stream<Student> streamStudents, Comparator<Student> comparator
    ) {
        return streamStudents.sorted(comparator).toList();
    }

    private List<Student> sortedListStudentByNameOfStream(Stream<Student> streamStudents) {
        return sortedListStudentsByComparator(
                streamStudents,
                Comparator.comparing(Student::getLastName)
                          .thenComparing(Student::getFirstName)
                          .reversed().thenComparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortedListStudentsByComparator(students.stream(), Comparator.comparingInt(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortedListStudentByNameOfStream(students.stream());
    }

/////////

    private <T> Stream<Student> streamStudentsByPredicateValue(
            Collection<Student> students, T predicateValue, Function<Student, T> getStudentValue
    ) {
        return students.stream().filter(student -> getStudentValue.apply(student).equals(predicateValue));
    }

    private <T> List<Student> findStudentsByPredicateValue(
            Collection<Student> students, T predicateValue, Function<Student, T> getStudentValue
    ) {
        return sortedListStudentByNameOfStream(streamStudentsByPredicateValue(students, predicateValue, getStudentValue));
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudentsByPredicateValue(students, name, Student::getFirstName);
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudentsByPredicateValue(students, name, Student::getLastName);
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findStudentsByPredicateValue(students, group, Student::getGroup);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return streamStudentsByPredicateValue(students, group, Student::getGroup)
                .collect(
                        Collectors.toMap(
                                Student::getLastName,
                                Student::getFirstName,
                                BinaryOperator.minBy(Comparator.naturalOrder())));
    }
}
