package info.kgeorgiy.ja.levitskii.bank;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Random;

public class BankImplTest {
    private final static String letters = "abcdefghijklmnopqrstuvxyz";
    private final static String numbers = "1234567890";
    private final static String fullAlphabet = letters + numbers;
    private final static Random random = new Random(4875043285743285204L);

    protected Bank bank = null;

    protected final static String NAME = "Иван";
    protected final static String LAST_NAME = "Левицкий";


    @BeforeClass
    public static void startRMIRegistry() {
        //Alternative:
        // start rmiregistry
        // java -cp info.kgeorgiy.ja.levitskiy.rmi.adapters.Server

        try {
            LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
            Server.main();
        } catch (RemoteException e) {
            System.out.println("Tests are crashing. Cannot start rmiregistry.");
        }
    }

    @Before
    public void createClient() {
        bank = Client.start();
        if (bank == null) {
            System.out.println("Tests are crashing. Cannot connect with running server.");
        }
    }

    protected Person createPerson(String name, String lastName, String passport) throws RemoteException {
        return bank.createPerson(name, lastName, passport);
    }

    protected Account createBankAccount(String id) throws RemoteException {
        return bank.createAccount(id);
    }

    protected Person getPerson(String passport, TypesOfPerson type) throws RemoteException {
        return bank.getPerson(passport, type);
    }

    protected Account getAccount(String id) throws RemoteException {
        return bank.getAccount(id);
    }



    protected static void assertAllNamingFields(String passport, Person person) throws RemoteException {
        Assert.assertEquals(BankImplTest.NAME, person.getFirstName());
        Assert.assertEquals(BankImplTest.LAST_NAME, person.getLastName());
        Assert.assertEquals(passport, person.getPassportNumber());
    }


    protected static String createRandomNotPersonAccountIdString(int size, String suffix) {
        StringBuilder sb = new StringBuilder();
        sb.append(letters.charAt(getRandomOnMod(letters.length())));
        generate(size, sb);
        sb.append(suffix);
        return sb.toString();
    }

    protected static String createSubId(String suffix) {
        StringBuilder sb = new StringBuilder();
        generate(10, sb);
        sb.append(suffix);
        return sb.toString();
    }

    protected PersonId createRandomPersonAccountId(String passportPrefix, String subIdSuffix) {
        String passport = getRandomPassport(passportPrefix);
        String subId = createSubId(subIdSuffix);
        return new PersonId(passport, subId);
    }

    protected static int getRandomOnMod(int x) {
        return ((random.nextInt() % x) + x) % x;
    }

    protected static String getRandomPassport(String prefix) {
        StringBuilder passport = new StringBuilder();
        passport.append(prefix);
        for (int i = prefix.length(); i < 10; i++) {
            passport.append(getRandomOnMod(10));
        }
        return passport.toString();
    }


    protected static class PersonId {
        public String passport;
        public String subId;

        public PersonId(String passport, String subId) {
            this.passport = passport;
            this.subId = subId;
        }

        @Override
        public String toString() {
            return passport + ":" + subId;
        }
    }


    private static void generate(int size, StringBuilder sb) {
        for (int i = 0; i < size; i++) {
            sb.append(fullAlphabet.charAt(getRandomOnMod(fullAlphabet.length())));
        }
    }
}
