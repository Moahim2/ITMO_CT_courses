package info.kgeorgiy.ja.levitskii.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

public interface Person extends Remote {

    String getFirstName() throws RemoteException;

    String getLastName() throws RemoteException;

    String getPassportNumber() throws RemoteException;

    Map<String, Account> getAccounts() throws RemoteException;

    Account createAccount(String subId) throws RemoteException;

    Account getAccount(String subId) throws RemoteException;
}

