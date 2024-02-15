package info.kgeorgiy.ja.levitskii.bank;

public class RemoteAccount implements Account {
    private final String id;
    private int amount;

    public RemoteAccount(final String id) {
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
