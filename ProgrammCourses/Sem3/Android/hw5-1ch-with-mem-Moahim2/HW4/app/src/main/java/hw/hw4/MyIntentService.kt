@file:Suppress("DEPRECATION")

package hw.hw4

import android.app.IntentService
import android.content.Intent
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.net.Uri
import android.os.Binder
import android.os.Bundle
import android.os.IBinder
import android.provider.MediaStore
import android.util.Log
import android.widget.Toast
import java.io.*
import java.net.HttpURLConnection
import java.net.URL
import java.util.concurrent.CopyOnWriteArrayList


@Suppress("DEPRECATION")
class MyIntentService : IntentService("1") {

    private lateinit var pathToPrivateStorage : String

    private var binder = MyBinder()
    var flagBlockLoadImage = false

    private var connection : HttpURLConnection? = null
    private var bufferedReader : BufferedReader? = null
    var lastID = 0


    var currentListMessage : CopyOnWriteArrayList<Message> = CopyOnWriteArrayList()


    var curBigBitMap : Bitmap? = null

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

                    //loadToDB
                    loadToDB()
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
                    }
                }
                "loadNew" -> { //readNew(Message)    //update
                    if (flagBlockLoadImage) {
                        return
                    }

                    connection =
                        URL("http://213.189.221.170:8008/1ch?lastKnownId=${lastID}").openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"
                    bufferedReader = BufferedReader(InputStreamReader(connection?.inputStream))

                    val lastAbsPos = lastID - currentListMessage[0].id

                    currentListMessage.addAll(parser.readAll(bufferedReader?.readLine()))

                    lastID = currentListMessage.last().id
                    returningIntent.action = "update"
                    bundleForReturningIntent.putInt("val", lastAbsPos)

                    bufferedReader!!.close()

                    loadToDB()
                }

                "loadMiniImage" -> { //readingMiniImage
                    if (flagBlockLoadImage) {
                        return
                    }

                    connection = URL(
                        "http://213.189.221.170:8008/thumb/" +
                                intent.getStringExtra("val")!!
                    ).openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"


                    val curMiniBitMap = BitmapFactory.decodeStream(connection?.inputStream)
                    //update in Service
                    val idCurMes = intent.getIntExtra("id", 0)
                    currentListMessage[idCurMes - currentListMessage[0].id].image = curMiniBitMap?.copy(curMiniBitMap.config, true)
                    currentListMessage[idCurMes - currentListMessage[0].id].text_message = null

                    //update in DB
                    saveImageToPrivateExternalStorage(
                        currentListMessage[idCurMes - currentListMessage[0].id])
                    val db = MyDBApp.staticInstance?.myDatabase!!
                    db.messageDao()?.update(currentListMessage[idCurMes - currentListMessage[0].id])

                    returningIntent.action = "miniImage"
                    bundleForReturningIntent.putInt("id", idCurMes)

                }

                "loadBigImage" -> { //readingBigImage
                    connection = URL(
                        "http://213.189.221.170:8008/img/" +
                                intent.getStringExtra("val")!!
                    ).openConnection() as HttpURLConnection
                    connection?.doInput = true
                    connection?.requestMethod = "GET"

                    curBigBitMap = BitmapFactory.decodeStream(connection?.inputStream)

                    returningIntent.action = "bigImage"
                }

                "restoreAll" -> { //restoreAtStoreService
                    println("RestoreAtStoreService") //
                    returningIntent.action = "restore"

                    sendBroadcast(returningIntent)
                    return
                }


                //HW5   !//??????????????????????????????????????????????????????????????????????
                "sendImage" -> { //sendImage
                    val imageName = intent.getStringExtra("val")
                    val imagePath = Uri.parse(imageName)
                    println(imagePath.path) //отладка

                    val bitmap = MediaStore.Images.Media.getBitmap(this.contentResolver, imagePath)
                    var imageArray: ByteArray?
                    ByteArrayOutputStream().use {
                        bitmap.compress(Bitmap.CompressFormat.JPEG, 90, it)
                        imageArray = it.toByteArray()
                    }
                    val imageFile = File(baseContext.cacheDir,
                        "image${System.currentTimeMillis()}.jpeg")
                    imageFile.outputStream().use {
                        it.write(imageArray)
                        it.flush()
                    }
                    MultipartExampleClient().mainF(arrayOf(
                        "http://213.189.221.170:8008/1ch",
                        "msg%${creater.makeImageMessage(imageFile.absolutePath)}",
                        "pic=${imageFile.absolutePath}"
                    ))
                    imageFile.delete()
                    return
                }

                "restoreAllAtDB" ->  { //restore without Internet at DB
                    returningIntent.action = "restoreAtDB"

                    val db = MyDBApp.staticInstance?.myDatabase!!

                    val dbDataList = db.messageDao()?.getAll()

                    if (dbDataList == null || dbDataList.isEmpty()) { // базы нет, это - первый запуск
                        bundleForReturningIntent.putBoolean("flag", false)
                        returningIntent.putExtras(bundleForReturningIntent)
                        sendBroadcast(returningIntent)
                        return
                    }

                    for (message in dbDataList) {
                        if (message.absolutelyPath != "") {
                            println(message.absolutelyPath) //отладка
                            File(message.absolutelyPath).inputStream().use {
                                message.image = BitmapFactory.decodeStream(it)
                            }
                        }
                    }
                    currentListMessage = CopyOnWriteArrayList(dbDataList)
                    lastID = currentListMessage.last().id

                    bundleForReturningIntent.putBoolean("flag", true)
                    returningIntent.putExtras(bundleForReturningIntent)
                    sendBroadcast(returningIntent)
                    return
                }

                "loadToDB" -> { //load all new information to DB (just in case))))//
                    loadToDB()
                    return
                }

                else -> { //HZ
                    return
                }
            }
        } catch (e1 : Exception) {
            Log.d("D", "LOOSE")
            println(e1.message)
            println(e1.stackTraceToString())
            Toast.makeText(this, "МЫ ПРОИГРАЛИ", Toast.LENGTH_SHORT).show()
            return
        }
        returningIntent.putExtras(bundleForReturningIntent)
        sendBroadcast(returningIntent)
        connection!!.disconnect()
    }


    @Deprecated("Deprecated in Java")
    override fun onCreate() {
        Log.d("INTENT_SERVICE", "CREATE")
        pathToPrivateStorage = getExternalFilesDir(null)?.path + File.separator + "myImages"
        val imagesDir = File(pathToPrivateStorage)
        if (!imagesDir.exists()) {
            imagesDir.mkdir()
        }
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
        Log.d("INTENT_SERVICE", "DISCONNECTED")
        if (connection != null) {
            connection?.disconnect()
        }
    }

//HW5!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    private fun loadToDB() {
        println("loadToDB")  //

        var ind = 0
        val db = MyDBApp.staticInstance?.myDatabase!!

        val lastIndex = db.messageDao()?.getMaxID()
        println("lastIDInDB : ${lastIndex?.id}, lastIndexInRecyclerView : $lastID ") //отладка

        if (lastIndex != null) {
            ind = lastIndex.id - currentListMessage[0].id + 1
        }


        for (i in (ind) until currentListMessage.size) {
            val message = currentListMessage[i]
            saveImageToPrivateExternalStorage(message)
            db.messageDao()?.insert(message)
        }
    }

    private fun saveImageToPrivateExternalStorage(message: Message) {
        if (message.image == null) {
            return
        }
        val absPathToImage =
            pathToPrivateStorage + File.separator +
                    System.currentTimeMillis() + "image.png"
        Log.d("INTENT_SERVICE", "loadToDB : $absPathToImage")
        File(absPathToImage).outputStream().use {
            message.image?.compress(Bitmap.CompressFormat.PNG, 100, it)
        }
        message.absolutelyPath = absPathToImage
    }
}