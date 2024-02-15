package info.kgeorgiy.java.advanced.student;

import java.util.Collection;
import java.util.List;

/**
 * Advanced-version interface
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-student">Student</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface AdvancedQuery extends GroupQuery {
    /**
     * Returns the first name of the student such that the most number of groups has student with that name.
     * If there is more than one such name, the least one is returned.
     */
    String getMostPopularName(Collection<Student> students);

    /** Returns student {@link Student#getFirstName() first names} by ids. */
    List<String> getFirstNames(Collection<Student> students, final int[] ids);

    /** Returns student {@link Student#getLastName() last names} by ids. */
    List<String> getLastNames(Collection<Student> students, final int[] ids);

    /** Returns student {@link Student#getGroup() groups} by ids. */
    List<GroupName> getGroups(Collection<Student> students, final int[] ids);

    /** Returns full student name by ids. */
    List<String> getFullNames(Collection<Student> students, final int[] ids);
}
