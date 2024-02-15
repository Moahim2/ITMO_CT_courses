package hw.hw4

import org.json.*

class ParserJSONMessages {

    fun readAll(input : String?) : ArrayList<Message> {
        val arrayMessages = JSONArray(input)
        val listMessage = ArrayList<Message>()

        for (i in 0 until arrayMessages.length()) {
            val arrayFields = arrayMessages.getJSONObject(i)
            val type = arrayFields.getJSONObject("data")
            if (type.optString("Image") == "") {
                listMessage.add(Message(
                    "NAME : ${arrayFields.getString("from")}," +
                            " UNIXTime : ${arrayFields.getString("time")}",
                    arrayFields.getJSONObject("data").getJSONObject("Text").getString("text"),
                    arrayFields.getString("id").toInt(),
                    null,
                    "",
                    ""
                    ))
            } else {
                listMessage.add(Message(
                    "NAME : ${arrayFields.getString("from")}," +
                            " UNIXTime : ${arrayFields.getString("time")}",
                    "...DownloadImage...",
                    arrayFields.getString("id").toInt(),
                    null,
                    arrayFields.getJSONObject("data").getJSONObject("Image").getString("link"),
                    ""
                ))
            }
        }
        return listMessage
    }
}
