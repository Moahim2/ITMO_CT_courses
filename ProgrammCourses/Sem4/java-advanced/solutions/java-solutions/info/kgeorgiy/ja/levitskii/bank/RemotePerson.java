package info.kgeorgiy.ja.levitskii.bank;

import java.rmi.RemoteException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;


public class RemotePerson implements Person {
    private final String firstName;
    private final String lastName;
    private final String passportNumber;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();

    private final Bank bank;


    public RemotePerson(final String firstName, final String lastName, final String passportNumber, final Bank bank) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.passportNumber = passportNumber;
        this.bank = bank;
    }

    @Override
    public synchronized String getFirstName() {
        return firstName;
    }

    @Override
    public synchronized String getLastName() {
        return lastName;
    }

    @Override
    public synchronized String getPassportNumber() {
        return passportNumber;
    }

    @Override
    public synchronized Map<String, Account> getAccounts() {
        return accounts;
    }

    @Override
    public synchronized Account createAccount(String subId) throws RemoteException {
        accounts.putIfAbsent(subId, bank.createAccount(passportNumber + ":" + subId));
        return accounts.get(subId);
    }

    @Override
    public synchronized Account getAccount(String subId) throws RemoteException {
        return accounts.get(subId);
    }
}
