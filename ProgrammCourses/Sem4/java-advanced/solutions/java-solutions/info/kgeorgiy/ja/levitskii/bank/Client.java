package info.kgeorgiy.ja.levitskii.bank;


import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public final class Client {
    /** Utility class. */
    public Client() {}

    public static void main(final String... args) {
        start();
    }

    public static Bank start() {
        Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
            return bank;
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
        } catch (final RemoteException e) {
            System.out.println("Registry could not be contacted");
        }
        return null;
    }
}
