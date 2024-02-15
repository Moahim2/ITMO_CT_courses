package hw.hw4

import org.json.*

class CreaterJSONMessages {
    fun makeSendTextMessage(text : String) : JSONObject {
        return JSONObject(
                "{\"from\":\"Moahim2\",\"to\"" +
                        ":\"1@channel\",\"data\":{\"Text\":{\"text\"" +
                        ":\"$text\"}}}")
    }

    fun makeImageMessage(text : String) : JSONObject {
        return JSONObject(
            "{\"from\":\"Moahim2\",\"to\"" +
                    ":\"1@channel\",\"data\":{\"Image\":{\"link\"" +
                    ":\"$text\"}}}")
    }
}