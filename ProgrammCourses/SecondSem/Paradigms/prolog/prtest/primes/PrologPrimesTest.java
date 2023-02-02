package prtest.primes;

import base.Selector;
import base.TestCounter;
import prtest.PrologScript;

import prtest.Rule;

import java.util.stream.IntStream;


import java.util.function.Consumer;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class PrologPrimesTest {
    public static final Selector SELECTOR = new Selector(PrologPrimesTest.class, "easy", "hard", "bonus")
            .variant("Primes", variant(t -> {}))
            .variant("Palindrome", variant(PrologPrimesTest::palindrome))
            .variant("Lcm", variant(PrologPrimesTest::lcm))
            .variant("Unique", variant(PrologPrimesTest::unique))
            .variant("Nth", variant(PrologPrimesTest::nth))


            ;

    private PrologPrimesTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }

    /* package-private */ static Consumer<TestCounter> variant(final Consumer<PrologPrimesTester> check) {
        return counter -> {
            final int mode = counter.mode();
            final int max = (int) (1000 * Math.pow(100.0 / TestCounter.DENOMINATOR, mode));
            new PrologPrimesTester(counter, max, mode > 0, check).test();
        };
    }

    // Palindrome
    private static final Rule PRIME_PALINDROME = new Rule("prime_palindrome", 2);

    private static void palindrome(final PrologPrimesTester t, final int n, final int radix) {
        t.assertSuccess(t.isPrime.get(n) && palindrome(n, radix), PRIME_PALINDROME, n, radix);
    }

    private static boolean palindrome(final int n, final int radix) {
        int reversed = 0;
        for (int value = n; value > 0; value /= radix) {
            reversed = reversed * radix + value % radix;
        }
        return n == reversed;
    }

    private static void palindrome(final PrologPrimesTester t) {
        for (int radix = 2; radix <= 10; radix++){
            for (int i = 1; i < 10; i++) {
                palindrome(t, i, radix);
            }
            for (final int prime : t.primes) {
                if (palindrome(prime, radix)) {
                    palindrome(t, prime, radix);
                }
            }
            for (int i = 0; i < 1000; i++) {
                palindrome(t, t.randomN(), radix);
            }
        }
    }

    private static long gcd(final long a, final long b) {
        return a == 0 ? b : gcd(b % a, a);
    }

    private static void lcm(final PrologPrimesTester t) {
        t.testBinary("lcm", (a, b) -> a * b / gcd(a, b));
    }
    
    // Unique
    public static final Rule UNIQUE_PRIME_DIVISORS = new Rule("unique_prime_divisors", 2);

    private static void unique(final PrologPrimesTester t) {
        t.checkDivisors(UNIQUE_PRIME_DIVISORS, false, IntStream::distinct);
    }

    // Nth
    private static final Rule NTH_PRIME = new Rule("nth_prime", 1 + 1);
    private static final Rule NTH = NTH_PRIME.func();
    private static final Rule NTH_REVERSE = NTH_PRIME.bind(0, PrologScript.V);

    private static void nth(final PrologPrimesTester t, final int i) {
        t.assertResult(t.primes[i], NTH, i + 1);
        if (t.reversible) {
            t.assertResult(i + 1, NTH_REVERSE, t.primes[i]);
        }
    }

    private static void nth(final PrologPrimesTester t) {
        for (int i = 0; i < 10; i++) {
            nth(t, i);
        }
        for (int i = 0; t.primes[i] * t.primes[i] < t.max * 10; i += 10) {
            nth(t, i);
        }
    }



}

