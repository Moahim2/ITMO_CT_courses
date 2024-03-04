package levitskiy.hw.controller.fxml;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.StackPane;
import javafx.stage.WindowEvent;
import javafx.util.Duration;
import levitskiy.hw.controller.AbstractUIClientController;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.ResourceBundle;

/**
 * Implementing the controller on the FXML java UI.
 */
public class FXMLClientController extends AbstractUIClientController implements Initializable {
    @FXML
    public TextField input1;
    @FXML
    public TextField input2;
    @FXML
    public TextArea errorInformationArea;
    @FXML
    public TextArea informationArea;

    private static final int ERROR_INFORMATION_SHOW_DELAY_SECONDS = 5;

    private final String hostName;
    private final int port;

    @FXML
    private StackPane rootPane;

    @FXML
    public void menuLogin() {
        setView("/processor/login.fxml");
    }

    @FXML
    public void menuUpload() {
        setView("/processor/upload.fxml");
    }

    @FXML
    public void menuDownload() {
        setView("/processor/download.fxml");
    }

    @FXML
    public void menuCopy() {
        setView("/processor/copy.fxml");
    }

    @FXML
    public void menuMove() {
        setView("/processor/move.fxml");
    }

    public FXMLClientController(String hostName, int port) {
        super();
        this.hostName = hostName;
        this.port = port;
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {}

    /**
     * Starts the client
     *
     * @param hostName hostName
     * @param port port
     */
    @Override
    public void runClientWorking(String hostName, int port) {
        clientService.run(hostName, port);
    }

    @Override
    public void showMessageAfterRequest(String responseMessage) {
        showServiceInformation(responseMessage);
    }

    @Override
    public void showServiceInformation(String... serviceMessages) {
        safeRun(() -> textAreaNewInfoOut(informationArea, serviceMessages));
    }

    @Override
    public void showErrorMessage(String... errorMessages) {
        safeRun(() -> textAreaNewInfoOut(errorInformationArea, errorMessages));
    }

    /**
     * {@inheritDoc}
     * Stops the UI with a delay {@value #ERROR_INFORMATION_SHOW_DELAY_SECONDS}
     * for the user to read the information.
     */
    @Override
    public void stopAllShow() {
        PauseTransition pause = new PauseTransition(Duration.seconds(ERROR_INFORMATION_SHOW_DELAY_SECONDS));
        pause.setOnFinished(n -> Platform.exit());
        pause.play();
    }

    @FXML
    public void login() {
        sendCommand("LOGIN");
    }

    @FXML
    public void upload() {
        sendCommand("UPLOAD");
    }

    @FXML
    public void download() {
        sendCommand("DOWNLOAD");
    }

    @FXML
    public void copy() {
        sendCommand("COPY");
    }

    @FXML
    public void move() {
        sendCommand("MOVE");
    }

    public EventHandler<WindowEvent> getCloseEventHandler() {
        return closeEventHandler;
    }

    /**
     * Swap menu item (ui for command processor) to {@code fxml}
     * @param fxml path to processor fxml resource.
     */
    private void setView(String fxml) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource(fxml));
            loader.setControllerFactory(type -> this);
            Parent view = loader.load();

            rootPane.getChildren().clear();
            rootPane.getChildren().add(view);
        } catch (IOException e) {
            throw new FXMLException("Error UI working", e);
        }
    }


    /**
     * Request command {@code commandType} to the server by clicking on button.
     * If there is no connection, it shuts down the entire application with some delay.
     *
     * @param commandType type of command.
     * @see CommandType#type()
     */
    private void sendCommand(String commandType) {
        if (!checkConnect()) {
            return;
        }
        clientService.sendRequest(clientInputProcessors.get(commandType)
                .processAndPackClientInput(this, input1.getText(), input2.getText())
        );
        input1.clear();
        input2.clear();
    }

    /**
     * Checks if there is a connection and if not, tries to connect.
     * @return final connect result.
     */
    private boolean checkConnect() {
        if (!clientService.isConnecting()) {
            runClientWorking(hostName, port);
            return clientService.isConnecting();
        }
        return true;
    }

    /**
     * Performing an action with the UI on a potentially different thread.
     * @param runnable action.
     */
    private void safeRun(Runnable runnable) {
        Platform.runLater(runnable);
    }

    private void textAreaNewInfoOut(TextArea textArea, String... infoMessages) {
        textArea.clear();
        Arrays.stream(infoMessages).forEach(textArea::appendText);
    }

    /**
     * Handler for closing app on a "cross".
     */
    private final EventHandler<WindowEvent> closeEventHandler = event -> clientService.close();
}
