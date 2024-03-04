package levitskiy.hw.controller.fxml;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.util.Callback;

import java.util.List;
import java.util.Map;

import static levitskiy.hw.util.ClientUtil.getClientMainArgs;
import static levitskiy.hw.util.Utils.checkIncorrectCMDArgs;

/**
 * Special runner for fxml UI with controller {@link FXMLClientController}.
 */
public class FXMLApplicationStarter extends Application {
    /**
     * Expects either an empty array of arguments (then default arguments will be substituted),
     * or an array of length 2, where:
     * the 1st arg - hostname,
     * the 2nd - port number.
     *
     * @param args cmd args.
     */
    public static void main(String[] args) {
        if (checkIncorrectCMDArgs(args)) {
            return;
        }
        Map.Entry<String, Integer> address = getClientMainArgs(args);
        if (address == null) {
            System.exit(1);
        }
        launch(address.getKey(), Integer.toString(address.getValue()));
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        List<String> args = getParameters().getRaw();

        Callback<Class<?>, Object> controllerFactory = type ->
                new FXMLClientController(args.get(0), Integer.parseInt(args.get(1)));

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/client.fxml"));
        loader.setControllerFactory(controllerFactory);
        Parent root = loader.load();

        FXMLClientController controller = loader.getController();

        primaryStage.setTitle("Client");
        primaryStage.setScene(new Scene(root, 600, 600));
        primaryStage.setOnCloseRequest(controller.getCloseEventHandler());
        primaryStage.show();
    }
}
