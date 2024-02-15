package info.kgeorgiy.ja.levitskii.bank;

import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.Objects;

public class ClientApp {

    private final static String FORMAT_ARGUMENTS =
            "Arguments must be only in form <firstName, lastName, passport, accountSubId, changeAmount>";

    private final static String REGULAR_FORMAT_PASSPORT = "^\\d{10}$";


    public static void main(String[] args) {
        if (checkBadCondition(args == null, "Array of arguments must be not null.", FORMAT_ARGUMENTS)) {
            return;
        }
        assert args != null;

        if (checkBadCondition(args.length != 5, "Incorrect number of arguments.", FORMAT_ARGUMENTS)) {
            return;
        }

        if (checkBadCondition(Arrays.stream(args).anyMatch(Objects::isNull), "Arguments must be not null", FORMAT_ARGUMENTS)) {
            return;
        }

        final String firstName = args[0];
        final String lastName = args[1];
        final String passport = args[2];


        if (checkBadCondition(!passport.matches(REGULAR_FORMAT_PASSPORT), "Incorrect passport number (must be xxxxxxxxxx)")) {
            return;
        }


        final String subId = args[3];
        try {
            final int amount = Integer.parseInt(args[4]);

            final Bank bank = Client.start();
            if (checkBadCondition(bank == null, "Fatal client connect. Cannot found running server.")) {
                return;
            }
            assert bank != null;

            Person person = bank.getPerson(passport, TypesOfPerson.REMOTE);
            if (person == null) {
                person = bank.createPerson(firstName, lastName, passport);
            } else {
                if (checkBadCondition(!person.getFirstName().equals(firstName), "The name does not match the passport number. Real name is " + person.getFirstName())) {
                    return;
                }
                if (checkBadCondition(!person.getLastName().equals(lastName), "The lastname does not match the passport number. Real lastname is " + person.getLastName())) {
                    return;
                }
            }


            final Account ac = person.createAccount(subId);
            ac.addAmount(amount);
            System.out.println("New amount is " + ac.getAmount());
        } catch (NumberFormatException e) {
            System.err.println("Incorrect format of amount. Change amount must be number, but was " + args[4] + ".");
            System.err.println(FORMAT_ARGUMENTS);
        } catch (RemoteException e) {
            System.err.println("A server communication error has occurred. Please, resend request.");
        }

    }

    private static boolean checkBadCondition(boolean cond, String... messages) {
        if (cond) {
            Arrays.stream(messages).forEach(System.err::println);
            return true;
        }
        return false;
    }
}
