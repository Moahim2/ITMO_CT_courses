package info.kgeorgiy.ja.levitskii.bank;

import java.rmi.*;

public interface Account extends Remote {
    /** Returns account identifier. */
    String getId() throws RemoteException;

    /** Returns amount of money in the account. */
    int getAmount() throws RemoteException;

    /** Adds amount of money in the account. */
    void addAmount(int amount) throws RemoteException;
}
