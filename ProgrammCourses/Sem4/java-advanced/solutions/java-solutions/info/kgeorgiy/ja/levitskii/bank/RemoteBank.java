package info.kgeorgiy.ja.levitskii.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();


    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        if (id.matches("^[0-9]{10}:.+$")) { //Физ лица
            final String[] splitId = id.split(":");
            final String passport = splitId[0];
            final String subId = splitId[1];
            final Person person = getPerson(passport, TypesOfPerson.REMOTE);
            if (person == null) {
                throw new IllegalArgumentException("A person with such a passport is not created.");
            }

            final Map<String, Account> mapPersonAccounts = person.getAccounts();
            final Account ac = createAccountImpl(passport + ":" + subId);
            mapPersonAccounts.putIfAbsent(subId, ac);
            return ac;
        } else { //Не Физ лица
            return createAccountImpl(id);
        }
    }

    private Account createAccountImpl(final String fullId) throws RemoteException {
        System.out.println("Creating account " + fullId);
        final Account account = new RemoteAccount(fullId);
        if (accounts.putIfAbsent(fullId, account) == null) {
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAccount(fullId);
        }
    }

    @Override
    public Account getAccount(final String id) {
        //System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }


    //my
    @Override
    public Person createPerson(String firstName, String lastName, String passportNumber) throws RemoteException {
        //System.out.println("Creating person: " + firstName + " " + lastName + " " + passportNumber);
        final Person person = new RemotePerson(firstName, lastName, passportNumber, this);
        if (!passportNumber.matches("^[0-9]{10}$")) {
            throw new IllegalArgumentException("Incorrect passport number");
        }

        if (persons.putIfAbsent(passportNumber, person) == null) {
            UnicastRemoteObject.exportObject(person, port);
            return person;
        } else {
            return getPerson(passportNumber, TypesOfPerson.REMOTE);
        }
    }

    @Override
    public Person getPerson(String passportNumber, TypesOfPerson type) throws RemoteException {
        //System.out.println("Retrieving + " + type.name() + "person " + passportNumber);
        Person person = persons.get(passportNumber);
        if (person == null) {
            return null;
        }

        switch (type) {
            case REMOTE -> {
                return person;
            }
            case LOCAL -> {
                return new LocalPerson(person);
            }
            default -> {
                return null;
            }
        }
    }


}
