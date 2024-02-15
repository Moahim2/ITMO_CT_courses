package info.kgeorgiy.ja.levitskii.bank;


import org.junit.*;
import org.junit.runners.MethodSorters;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class ClientAppTest extends BankImplTest {

    @Test
    public void test1_nullArguments() throws RemoteException {
        runClientApp((String[]) null);
        runClientApp(null, null);

        String passport = getRandomPassport("01");
        runClientApp("1", "2", passport, null, "5");

        Assert.assertNull(getPerson(passport, TypesOfPerson.REMOTE));
    }

    @Test
    public void test2_BadLengthArgument() throws RemoteException {
        String passport = getRandomPassport("20");
        runClientApp(NAME);
        runClientApp(NAME, LAST_NAME);
        runClientApp(NAME, LAST_NAME, passport);
        Assert.assertNull(getPerson(passport, TypesOfPerson.REMOTE));

        ArrayList<String> list = new ArrayList<>();
        list.add(NAME);
        list.add(LAST_NAME);
        list.add(passport);
        list.add("1");
        list.add("10");
        for (int i = 0; i < 20; i++) {
            String[] arr = new String[i + 6];
            list.add(createRandomNotPersonAccountIdString(10, "test2_BadLengthArgument"));
            runClientApp(list.toArray(arr));
            Assert.assertNull(getPerson(passport, TypesOfPerson.REMOTE));
        }
    }

    @Test
    public void test3_BadPassport() throws RemoteException {
        for (int i = 0; i < 10; i++) {
            String passport = createRandomNotPersonAccountIdString(10, "");
            runClientApp(NAME, LAST_NAME, passport, "1", "10");
            Assert.assertNull(bank.getPerson(passport, TypesOfPerson.REMOTE));
        }
    }


    @Test
    public void test4_MismatchNames() throws RemoteException {
        String passport = getRandomPassport("40");
        createPerson(NAME, LAST_NAME, passport);
        Account ac =
                createBankAccount(createRandomPersonAccountId(passport, "test4_MismatchNames").toString());
        ac.addAmount(100);

        runClientApp("Ваня", "Левицкий", passport, "1", "200");
        Assert.assertEquals(100, ac.getAmount());

        runClientApp("Иван", "Левикий", passport, "1", "200");
        Assert.assertEquals(100, ac.getAmount());
    }

    @Test
    public void test5_IncorrectFormatAmount() throws RemoteException {
        String passport = getRandomPassport("50");
        createPerson(NAME, LAST_NAME, passport);
        runClientApp(NAME, LAST_NAME, passport, "1", "xyz1111");
    }

    @Test
    public void test6_NewClient() throws RemoteException {
        String passport = getRandomPassport("60");
        Assert.assertNull(getPerson(passport, TypesOfPerson.REMOTE));

        runClientApp(NAME, LAST_NAME, passport, "1", "10");
        Person person = getPerson(passport, TypesOfPerson.REMOTE);

        Assert.assertNotNull(person);
        assertAllNamingFields(passport, person);

        Assert.assertEquals(10, person.getAccount("1").getAmount());
    }


    @Test
    public void test7_NewAccounts() throws RemoteException {
        String passport = getRandomPassport("70");
        Person person = createPerson(NAME, LAST_NAME, passport);

        for (int i = 0; i < 10; i++) {
            String val = Integer.toString(i);
            Assert.assertNull(getAccount(passport + ":" + val));
            runClientApp(NAME, LAST_NAME, passport, val, Integer.toString(10 * i));
        }

        for (int i = 0; i < 10 ; i++) {
            String val = Integer.toString(i);
            Assert.assertEquals(10 * i, person.getAccount(val).getAmount());
        }
    }

    @Test
    public void test8_OneClientChangeAmount() throws RemoteException {
        testConcurrentClientsChangeAmount("80", "test8_OneClientChangeAmount_", 1);
    }

    @Test
    public void test9_ManyClientsChangeAmount() throws RemoteException {
        testConcurrentClientsChangeAmount("90", "test9_ManyClientsChangeAmount_", 10);
    }



    private void testConcurrentClientsChangeAmount(String passportPrefix, String idSuffix, int count) throws RemoteException {
        Map<String, Integer> passports = new ConcurrentHashMap<>();
        try (ExecutorService executorService = Executors.newFixedThreadPool(count)) {
            for (int i = 0; i < count; i++) {
                int finalI = i;
                executorService.execute(() -> {
                    final String passport = getRandomPassport(passportPrefix + finalI);

                    String name = createRandomNotPersonAccountIdString(10, idSuffix);
                    String lastName = createRandomNotPersonAccountIdString(20, idSuffix);

                    final String changeAmount = Integer.toString(finalI);
                    final String antiChangeAmount = Integer.toString(-finalI);
                    for (int j = 0; j <= finalI; j++) {
                        runClientApp(name, lastName, passport, "1", changeAmount);
                        runClientApp(name, lastName, passport, "2", "0");
                        runClientApp(name, lastName, passport, "3", antiChangeAmount);
                    }
                    passports.put(passport, finalI);
                });
            }

            executorService.shutdownNow();
        }

        for (String passport : passports.keySet()) {
            Person person = getPerson(passport, TypesOfPerson.REMOTE);
            Assert.assertNotNull(person);

            final int value = passports.get(passport);

            Assert.assertEquals(value * (value + 1), person.getAccount("1").getAmount());
            Assert.assertEquals(0, person.getAccount("2").getAmount());
            Assert.assertEquals(-value * (value + 1), person.getAccount("3").getAmount());
        }
    }



    private void runClientApp(String... args) {
        ClientApp.main(args);
    }
}
