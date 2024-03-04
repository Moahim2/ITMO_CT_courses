package levitskiy.hw.dao;

import java.util.Map;

/**
 * Emulation of user data stored in the server storage.
 */
public class UserDao {
    private static final Map<String, String> users = Map.of(
            "IvanLevitskiy", "Moahim2003",
            "fff", "fff",
            "OldUser", "123"
    );

    public static String getUserPassword(String login) {
        return users.get(login);
    }
}
