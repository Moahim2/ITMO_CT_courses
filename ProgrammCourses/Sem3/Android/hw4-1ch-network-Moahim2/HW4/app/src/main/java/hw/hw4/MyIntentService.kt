@file:Suppress("DEPRECATION")

package hw.hw4

import android.app.IntentService
import android.content.Intent
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Binder
import android.os.Bundle
import android.os.IBinder
import android.util.Log
import android.widget.Toast
import java.io.*
import java.net.HttpURLConnection
import java.net.URL


@Suppress("DEPRECATION")
class MyIntentService : IntentService("1") {
    private var binder = MyBinder()

    private var connection : HttpURLConnection? = null
    private var bufferedReader : BufferedReader? = null
    var lastID = 0


    var currentListMessage : ArrayList<Message> = ArrayList()
    var listNewMessages : ArrayList<Message> = ArrayList()

    var curMiniBitMap : Bitmap? = null
    var curBigByteArray : ByteArray? = null

    @Deprecated("Deprecated in Java")
    override fun onHandleIntent(intent: Intent?) {
        val parser = ParserJSONMessages()
        val creater = CreaterJSONMessages()

        val returningIntent = Intent()
        val bundleForReturningIntent = Bundle()

        try {

            when (intent?.getStringExtra("type")) {
                "readAll" -> { //readAll(Text + &image)

                    var count = 0
                    var newMessages = ArrayList<Message>()
                    while (newMessages.size != 0 || count == 0) {
                        connection =
                            URL(
                                "http://213.189.221.170:8008/1ch?lastKnownId=${count + 1}" +
                                        "&limit=30").openConnection() as HttpURLConnection
                        connection?.doInput = true
                        connection?.requestMethod = "GET"
                        bufferedReader = BufferedReader(InputStreamReader(connection?.inputStream))
                        newMessages = parser.readAll(bufferedReader?.readLine())
                        count += newMessages.size
                        bufferedReader!!.close()
                        connection?.disconnect()
                        currentListMessage.addAll(newMessages)
                    }

                    lastID = currentListMessage.last().id

                    returningIntent.action = "loadAll"

                    bufferedReader!!.close()
                }
                "send" -> { //sendText
                    connection =
                        URL("http://213.189.221.170:8008/1ch").openConnection() as HttpURLConnection
                    connection?.doOutput = true
                    connection?.setRequestProperty("Content-Type", "application/json")
                    connection?.requestMethod = "POST"


                    val out = OutputStreamWriter(connection?.outputStream)
                    out.write(
                        creater.makeSendTextMessage(intent.getStringExtra("val")!!).toString()
                    )
                    out.close()
                    if (connection!!.responseCode != 200) {
                        Toast.makeText(this, "МЫ ПРОИГРАЛИ", Toast.LENGTH_SHORT).show()
                        //БЕЗ этого не работает соре
                    }
                }
                "loadNew" -> { //readNew(Text)
                    connection =
                        URL("http://213.189.221.170:8008/1ch?lastKnownId=${lastID}").openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"
                    bufferedReader = BufferedReader(InputStreamReader(connection?.inputStream))


                    listNewMessages = parser.readAll(bufferedReader?.readLine())
                    for (json in listNewMessages) {
                        if (listNewMessages.last() == json) {
                            lastID = json.id
                        }
                        currentListMessage.add(json)
                    }


                    returningIntent.action = "update"

                    bufferedReader!!.close()
                }

                "loadMiniImage" -> { //readingMiniImage
                    connection = URL(
                        "http://213.189.221.170:8008/thumb/" +
                                intent.getStringExtra("val")!!
                    ).openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"


                    curMiniBitMap = BitmapFactory.decodeStream(connection?.inputStream)
                    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    val idCurMes = intent.getIntExtra("id", 0)
                    currentListMessage[idCurMes - currentListMessage[0].id].image = curMiniBitMap
                    currentListMessage[idCurMes - currentListMessage[0].id].text_message = null

                    returningIntent.action = "miniImage"
                    bundleForReturningIntent.putInt("id", idCurMes)

                }

                "loadBigImage" -> {
                    connection = URL(
                        "http://213.189.221.170:8008/img/" +
                                intent.getStringExtra("val")!!
                    ).openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"

                    val bitmap = BitmapFactory.decodeStream(connection?.inputStream)
                    val byteCodeStreamDecode = ByteArrayOutputStream()
                    bitmap.compress(Bitmap.CompressFormat.PNG, 100, byteCodeStreamDecode)
                    curBigByteArray = byteCodeStreamDecode.toByteArray()

                    returningIntent.action = "bigImage"

                    byteCodeStreamDecode.close()
                }

                "restoreAll" -> {
                    returningIntent.action = "restore"

                    sendBroadcast(returningIntent)
                    return
                }
                else -> {
                    return
                }
            }
        } catch (e1 : Exception) {
            Log.d("D", "LOOSE")
            Toast.makeText(this, "МЫ ПРОИГРАЛИ", Toast.LENGTH_SHORT).show()
        }
        returningIntent.putExtras(bundleForReturningIntent)
        sendBroadcast(returningIntent)
        connection!!.disconnect()
    }



    @Deprecated("Deprecated in Java")
    override fun onCreate() {
        Log.d("IntentSERVICE", "CREATE")
        super.onCreate()
    }

    @Deprecated("Deprecated in Java")
    override fun onBind(intent: Intent?): IBinder {
        return binder
    }


    inner class MyBinder : Binder() {
        fun getMyService() = this@MyIntentService
    }


    @Deprecated("Deprecated in Java")
    override fun onDestroy() {
        super.onDestroy()
        Log.d("SERVICE", "DISCONNECTED")
        connection?.disconnect()
    }

}