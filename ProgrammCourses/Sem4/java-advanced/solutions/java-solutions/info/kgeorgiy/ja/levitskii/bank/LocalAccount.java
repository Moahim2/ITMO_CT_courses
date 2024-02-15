package info.kgeorgiy.ja.levitskii.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

public class LocalAccount implements Account, Serializable {
    private final String id;
    private int amount;

    public LocalAccount(Account account) throws RemoteException {
        id = account.getId();
        amount = account.getAmount();
    }

    public LocalAccount(final String id) {
        this.id = id;
        amount = 0;
    }

    @Override
    public synchronized String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        //System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    @Override
    public synchronized void addAmount(final int amount) {
        //System.out.println("Setting amount of money for account " + id);
        this.amount += amount;
    }
}
