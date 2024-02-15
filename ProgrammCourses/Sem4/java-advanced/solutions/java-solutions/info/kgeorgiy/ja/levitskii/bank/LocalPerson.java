package info.kgeorgiy.ja.levitskii.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class LocalPerson implements Person, Serializable {
    private final String firstName;
    private final String lastName;
    private final String passportNumber;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();


    public LocalPerson(Person person) throws RemoteException {
        this.firstName = person.getFirstName();
        this.lastName = person.getLastName();
        this.passportNumber = person.getPassportNumber();

        Map<String, Account> personAccounts = person.getAccounts();
        for (var ac : personAccounts.keySet()) {
            accounts.put(ac, new LocalAccount(personAccounts.get(ac)));
        }
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
    public synchronized Account createAccount(String subId) {
        accounts.putIfAbsent(subId, new LocalAccount(passportNumber + ":" + subId));
        return accounts.get(subId);
    }

    @Override
    public synchronized Account getAccount(String subId) throws RemoteException {
        return accounts.get(subId);
    }
}
