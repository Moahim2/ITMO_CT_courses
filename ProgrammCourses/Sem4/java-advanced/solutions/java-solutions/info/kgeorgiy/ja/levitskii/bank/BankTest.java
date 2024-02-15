package info.kgeorgiy.ja.levitskii.bank;


import org.junit.*;
import org.junit.runners.MethodSorters;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Consumer;


@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class BankTest extends BankImplTest {


    @Test
    public void test1_NoPerson() throws RemoteException {
        testNoPerson("10","test1_NoPerson_", 100, 1);
    }

    @Test
    public void test2_ConcurrentNoPerson() throws RemoteException {
        testNoPerson("20","test2_ConcurrentNoPerson_", 1000, 5);
    }



    @Test
    public void test3_OnePerson() throws RemoteException {
        String passport = "3000000000";
        Person person = createPerson(NAME, LAST_NAME, "3000000000");
        assertAllNamingFields(passport, person);
        assertAllNamingFields(passport, getPerson("3000000000", TypesOfPerson.REMOTE));
        Assert.assertTrue(person.getAccounts().isEmpty());
    }

    @Test
    public void test4_OnePersonAccount() throws RemoteException {
        Person person = createPerson(NAME, LAST_NAME, "4000000000");
        String subId = createSubId("test4_OnePersonAccount_");
        person.createAccount(subId);
        person.getAccount(subId).addAmount(100);
        Assert.assertEquals(100, getAccount("4000000000" + ":" + subId).getAmount());

        for (int i = 0; i < 10; i++) {
            person.getAccount(subId).addAmount(10);
        }
        Assert.assertEquals(200, person.getAccount(subId).getAmount());
    }

    @Test
    public void test5_OnePersonLocalAndRemote() throws RemoteException {
        final String passportPrefix = "50";
        final String idSuffix = "test5_OnePersonLocalAndRemote_";

        String passport = getRandomPassport(passportPrefix);
        createPerson(
                createRandomNotPersonAccountIdString(10, ""),
                createRandomNotPersonAccountIdString(20, ""),
                passport
        );
        Person remotePerson = getPerson(passport, TypesOfPerson.REMOTE);
        PersonId personId = createRandomPersonAccountId(passport, idSuffix);
        Account ac = createBankAccount(personId.toString());

        ac.addAmount(100);
        Account acPerson = remotePerson.getAccount(personId.subId);
        Assert.assertEquals(100, acPerson.getAmount());

        Person localPerson = getPerson(passport, TypesOfPerson.LOCAL);
        Assert.assertEquals(100, localPerson.getAccount(personId.subId).getAmount());
        localPerson.getAccount(personId.subId).addAmount(100);
        Assert.assertEquals(200, localPerson.getAccount(personId.subId).getAmount());
        Assert.assertEquals(100, acPerson.getAmount());
        acPerson.addAmount(500);

        Assert.assertEquals(200, localPerson.getAccount(personId.subId).getAmount());
        Assert.assertEquals(600, getAccount(personId.toString()).getAmount());
    }

    @Test
    public void test6_ManyRemotePersonsOneAccount() throws RemoteException {
        multiTestPersons("60", "test6_ManyRemotePersonsOneAccount_", 100, 1, 1, false);
    }

    @Test
    public void test7_ManyRemotePersonsManyAccountsIncrement() throws RemoteException {
        multiTestPersons("70", "test6_ManyRemotePersonsOneAccount_", 100, 10, 1, false);
    }

    @Test
    public void test8_ManyRemoteAndLocalPersonsManyAccountsIncrement() throws RemoteException {
        multiTestPersons("80", "test8_ManyRemoteAndLocalPersonsManyAccountsIncrement_", 500, 10, 1, true);
    }

    @Test
    public void test9_ManyRemoteAndLocalPersonsConcurrent() throws RemoteException {
        multiTestPersons("90", "test9_ManyRemoteAndLocalPersonsConcurrent_", 250, 10, 5, true);
    }







    private void testNoPerson(String passportPrefix, String idSuffix, int count, int threads) throws RemoteException {
        ArrayList<Account> accounts = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Account ac = createBankAccount(createRandomNotPersonAccountIdString(i, idSuffix));
            Assert.assertNotNull(ac);
            accounts.add(ac);
            ac.addAmount(i);
        }

        concurrentRun(threads, null, null, l -> {}, (l, map1, map2, setThreadExceptions) -> {
            for (int j = 0; j < count; j++) {
                try {
                    accounts.get(j).addAmount(j);
                } catch (RemoteException e) {
                    setThreadExceptions.add(e);
                }
            }
        });

        try {
            accounts.add(createBankAccount(createRandomPersonAccountId(passportPrefix, idSuffix).toString()));
            Assert.fail("Expected: IllegalArgumentException, because we haven't Person's accounts.");
        } catch (IllegalArgumentException e) {
            Assert.assertEquals("","A person with such a passport is not created.", e.getMessage());
        }

        for (int i = 0; i < count; i++) {
            Account ac = accounts.get(i);
            Assert.assertEquals(ac.getAmount(), getAccount(ac.getId()).getAmount());
            ac.addAmount(i);
        }

        for (int i = 0; i < count; i++) {
            Assert.assertEquals((2 + threads) * i, getAccount(accounts.get(i).getId()).getAmount());
        }
    }




    private void multiTestPersons(
            String passportPrefix, String idSuffix, int count, int accounts,
            int threads, boolean testLocal
    ) throws RemoteException {
        ArrayList<Map<String, Person>> listMapsRemotePersons = new ArrayList<>();
        final int amount = 10;

        concurrentRun(
            threads,
            listMapsRemotePersons,
            null,
            list -> list.add(new ConcurrentHashMap<>()),
            (l1, remotePersons, m2, setThreadExceptions) ->
                    createAccounts(passportPrefix, idSuffix, count, accounts, remotePersons, setThreadExceptions)
        );

        for (Map<String, Person> persons : listMapsRemotePersons) {
            Assert.assertEquals(count, persons.size());
            for (Person p : persons.values()) {
                Assert.assertEquals(
                        "Bad creating account: ", amount, p.getAccounts().values().iterator().next().getAmount()
                );
            }
        }

        concurrentRun(accounts, listMapsRemotePersons, null, l -> {}, (listMaps, n, m2, setThreadExceptions) -> {
            for (Map<String, Person> persons : listMapsRemotePersons) {
                for (Person person : persons.values()) {
                    try {
                        for (Account account : person.getAccounts().values()) {
                            account.addAmount(10);
                        }
                    } catch (RemoteException e) {
                        setThreadExceptions.add(e);
                    }
                }
            }
        });

        for (Map<String, Person> listMapsRemotePerson : listMapsRemotePersons) {
            for (Person p : listMapsRemotePerson.values()) {
                for (Account account : p.getAccounts().values()) {
                    Assert.assertEquals("Bad change amount", 10 + 10L * accounts, account.getAmount());
                }
            }
        }
        testLocalPersons(listMapsRemotePersons, testLocal);
    }

    private void testLocalPersons(ArrayList<Map<String, Person>> listRemotePersons, boolean flag) throws RemoteException {
        if (!flag) {
            return;
        }

        ArrayList<Map<String, Person>> listLocalPersons = new ArrayList<>();

        concurrentRun(
                listRemotePersons.size(),
                listLocalPersons,
                listRemotePersons,
                list -> list.add(new ConcurrentHashMap<>()),
                (l1, localPersons, remotePersons, setThreadExceptions) -> {
                    for (String passport : remotePersons.keySet()) {
                        try {
                            localPersons.put(passport, getPerson(passport, TypesOfPerson.LOCAL));
                            for (Account ac : localPersons.get(passport).getAccounts().values()) {
                                ac.addAmount(10); //local change
                            }
                        } catch (RemoteException e) {
                            setThreadExceptions.add(e);
                        }
                    }
                }
        );

        for (int i = 0; i < listRemotePersons.size(); i++) {
            Map<String, Person> remotePersons = listRemotePersons.get(i);
            for (String passport :remotePersons.keySet()) {
                Person localPerson = listLocalPersons.get(i).get(passport);
                for (Account ac : remotePersons.get(passport).getAccounts().values()) {
                    Assert.assertEquals("Local changes are visible globally",
                            ac.getAmount() + 10,
                            localPerson.getAccount(ac.getId().split(":")[1]).getAmount()
                    );
                }
            }
        }

        concurrentRun(
                listRemotePersons.size(),
                listLocalPersons,
                listRemotePersons,
                list -> list.add(new ConcurrentHashMap<>()),
                (l1, localPersons, remotePersons, setThreadExceptions) -> {
                    for (String passport : remotePersons.keySet()) {
                        try {
                            for (Account ac : remotePersons.get(passport).getAccounts().values()) {
                                ac.addAmount(-20); //global change
                            }
                        } catch (RemoteException e) {
                            setThreadExceptions.add(e);
                        }
                    }
                }
        );

        for (int i = 0; i < listRemotePersons.size(); i++) {
            Map<String, Person> remotePersons = listRemotePersons.get(i);
            for (String passport :remotePersons.keySet()) {
                Person localPerson = listLocalPersons.get(i).get(passport);
                for (Account ac : remotePersons.get(passport).getAccounts().values()) {
                    Assert.assertEquals("Global changes are visible locally",
                            ac.getAmount() + 10,
                            localPerson.getAccount(ac.getId().split(":")[1]).getAmount() - 20
                    );
                }
            }
        }
    }


    private void concurrentRun(int threads, ArrayList<Map<String, Person>> listMaps1,
                               ArrayList<Map<String, Person>> listMaps2,
                               Consumer<ArrayList<Map<String, Person>>> fillList, Task task) {
        Set<Throwable> setThreadExceptions = ConcurrentHashMap.newKeySet();
        try (ExecutorService executorService = Executors.newFixedThreadPool(threads)) {
            for (int i = 0; i < threads; i++) {
                fillList.accept(listMaps1);

                Map<String, Person> map1 = getPersonMap(listMaps1, i);
                Map<String, Person> map2 = getPersonMap(listMaps2, i);

                executorService.execute(() -> task.run(listMaps1, map1, map2, setThreadExceptions));
            }
            executorService.shutdownNow();
        }
        Assert.assertTrue("RemoteException was in concurrent threads", setThreadExceptions.isEmpty());
    }

    private static Map<String, Person> getPersonMap(ArrayList<Map<String, Person>> listMaps, int i) {
        Map<String, Person> map;
        if (listMaps == null || listMaps.size() <= i) {
            map = null;
        } else {
            map = listMaps.get(i);
        }
        return map;
    }


    @FunctionalInterface
    private interface Task {
        void run(
                ArrayList<Map<String, Person>> list,
                Map<String, Person> map1,
                Map<String, Person> map2,
                Set<Throwable> setExceptions
        );
    }


    private void createAccounts(String passportPrefix, String idSuffix, int count,
                                int accounts, Map<String, Person> remotePersons, Set<Throwable> setThreadExceptions) {
        for (int j = 0; j < count; j++) {
            String passport = getRandomPassport(passportPrefix + j);
            try {
                remotePersons.put(
                        passport,
                        createPerson(
                                createRandomNotPersonAccountIdString(10, idSuffix),
                                createRandomNotPersonAccountIdString(20, idSuffix),
                                passport
                        )
                );
            } catch (RemoteException e) {
                setThreadExceptions.add(e);
            }
            final Person person = remotePersons.get(passport);

            try {
                for (int k = 0; k < accounts; k++) {
                    person.createAccount(createSubId(idSuffix)).addAmount(10);
                }
            } catch (RemoteException e) {
                setThreadExceptions.add(e);
            }
        }
    }

}
