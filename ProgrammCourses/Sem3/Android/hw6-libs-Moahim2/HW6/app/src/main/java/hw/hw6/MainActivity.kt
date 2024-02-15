package hw.hw6

import android.app.Activity
import android.content.ComponentName
import android.content.Intent
import android.content.ServiceConnection
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.ImageDecoder
import android.os.Bundle
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.util.Log
import android.widget.Button
import android.widget.EditText
import android.widget.Toast
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import kotlinx.coroutines.*
import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.IOException


class MainActivity : AppCompatActivity() {

    private var drawRecyclerViewJob: Job? = null
    private lateinit var pathToPrivateStorage: String
    private val sqw = "MAIN_ACTIVITY"

    private var MY_KEY_ONE = "RESTORE_BOOLEAN"
    private var flagFirstStartActivity = true
    private var flagOnCreate = false

    private var globalLastPos = -1
    private var lastPos = -1
    private lateinit var listAllMessage : ArrayList<Message>



    private val viewManager = LinearLayoutManager(this)
    private lateinit var listMessage : RecyclerView
    private lateinit var messagesAdapter : MessageAdapter
    private lateinit var editText : EditText
    private lateinit var swipeRefreshLayout : SwipeRefreshLayout
    private lateinit var runnable : java.lang.Runnable
    private lateinit var handler : Handler


    private lateinit var scope : CoroutineScope
    private lateinit var serversWorkService : ServersWorkService




    var myStoreService : MyStoreService? = null
    var isStoreBound = false
    private val boundMyStoreServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            Log.d(sqw, "SERVICE_STORE_CONNECT")
            val binderBridge: MyStoreService.MyStoreBinder = service as MyStoreService.MyStoreBinder
            myStoreService = binderBridge.getMyService()
            isStoreBound = true
            drawRecyclerView()
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d(sqw, "SERVICE_STORE_DISCONNECT")
            isStoreBound = false
            myStoreService = null
        }
    }


    override fun onResume() {
        super.onResume()
        Log.d(sqw, "ON_RESUME")
        scope = CoroutineScope(Dispatchers.Main)
        //scope
        if (!flagOnCreate) {
            scope.launch {
                readAllNewMessage() //дочитываем все и дозагружаем
                for (i in 0 until listAllMessage.size - 1) {
                    loadMiniImage(listAllMessage[i])
                }
            }
        }
        flagOnCreate = false
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.d(sqw, "ON_CREATE")
        setContentView(R.layout.activity_main)
        if (savedInstanceState != null) {
            flagFirstStartActivity = savedInstanceState.getBoolean(MY_KEY_ONE, true)
        }

        pathToPrivateStorage = getExternalFilesDir(null)?.path + File.separator + "myImages"
        val imagesDir = File(pathToPrivateStorage)
        if (!imagesDir.exists()) {
            imagesDir.mkdir()
        }
        flagOnCreate = true
        createActivity()
    }






    private fun createActivity() {
        swipeRefreshLayout = findViewById(R.id.swipeRefreshLayot)
        listMessage = findViewById(R.id.list_messages)
        viewManager.reverseLayout = true
        viewManager.stackFromEnd = true

        serversWorkService = MyApp.staticInstance?.serversWorkService!!

        createSendingMessage()
        createRefreshingMessages()
        createSendingImage()
    }

    private fun createSendingMessage() {
        val buttonSendText = findViewById<Button>(R.id.button_for_send_text)
        editText = findViewById(R.id.new_message)
        buttonSendText.setOnClickListener {
            scope.launch {
                val response = withContext(Dispatchers.IO) {
                    try {
                        serversWorkService.sendTextMessage(
                            ToServerJSONMessage(
                                "Moahim2",
                                "1@channel",
                                mapOf(Pair("Text", mapOf(Pair("text", editText.text.toString()))))
                            )
                        ).execute()
                    } catch (e : Exception) {
                        localeExceptions(e, "(send_text_message)")
                        null
                    }
                } ?: return@launch
                localeHTTPProblems(response, "(send_text_message)")
                editText.setText("")
            }
        }
    }

    private fun createRefreshingMessages() {
        handler = Handler(Looper.getMainLooper())
        swipeRefreshLayout.setOnRefreshListener {
            runnable = Runnable {
                swipeRefreshLayout.isRefreshing = false

                scope.launch {
                    readAllNewMessage()
                }
            }
            handler.postDelayed(runnable, 1.toLong())
        }
    }


    private var resultLauncherOfSendingImage = registerForActivityResult(
        ActivityResultContracts.StartActivityForResult()
    )
    { res ->
        if (res.resultCode == Activity.RESULT_OK) {
            val uriImage = res.data?.data
            if (uriImage == null) {
                Toast.makeText(
                    this@MainActivity,
                    "Путь до картинки не получен...",
                    Toast.LENGTH_SHORT
                ).show()
                return@registerForActivityResult
            }

            scope.launch {
                val response = withContext(Dispatchers.IO) {
                    try {
                        val bitmap = ImageDecoder.decodeBitmap(
                            ImageDecoder.createSource(
                                this@MainActivity.contentResolver,
                                uriImage
                            )
                        )
                        var imageArray: ByteArray?
                        ByteArrayOutputStream().use {
                            bitmap.compress(Bitmap.CompressFormat.JPEG, 90, it)
                            imageArray = it.toByteArray()
                        }
                        val imageFile = File(
                            baseContext.cacheDir,
                            "image${System.currentTimeMillis()}.jpeg"
                        )
                        imageFile.outputStream().use {
                            it.write(imageArray)
                            it.flush()
                        }

                        val json = MyApp.staticInstance?.moshiWriterSendingMessage?.toJson(
                            ToServerJSONMessage(
                                "Moahim2",
                                "1@channel",
                                mapOf(
                                    Pair(
                                        "Image",
                                        mapOf(Pair("link", imageFile.absolutePath))
                                    )
                                )
                            )
                        )

                        val msg = RequestBody.create(
                            MediaType.parse("application/json; charset=utf-8"),
                            json!!
                        )

                        val body = MultipartBody.Part.createFormData(
                            "pic",
                            imageFile.name,
                            RequestBody.create(MediaType.parse("image/jpeg"), imageFile)
                        )
                        serversWorkService.sendImageMessage(msg, body).execute()
                    } catch (e : Exception) {
                        localeExceptions(e, "(send_image_message)")
                        null
                    }
                } ?: return@launch

                localeHTTPProblems(response, "(send_image_message)")
            }
        }
    }

    private fun createSendingImage() {
        val buttonSendImage = findViewById<Button>(R.id.button_for_send_image)
        buttonSendImage.setOnClickListener {
            val imagePick = Intent(Intent.ACTION_PICK)
            imagePick.type = "image/*"
            resultLauncherOfSendingImage.launch(imagePick)
        }
    }


    override fun onStart() {
        super.onStart()

        val starterMyStoreService = Intent(this, MyStoreService::class.java)
        startService(starterMyStoreService)
        bindService(starterMyStoreService, boundMyStoreServiceConnection, BIND_AUTO_CREATE)
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putBoolean(MY_KEY_ONE, flagFirstStartActivity)
        Log.d(sqw, "ON_SAVE_INSTANCE_STATE")
    }

    override fun onPause() {
        Log.d(sqw, "ON_PAUSE")
//        scope.cancel()
        if (drawRecyclerViewJob != null && drawRecyclerViewJob!!.isActive) {
            drawRecyclerViewJob!!.cancel()
        }
        super.onPause()
    }

    override fun onStop() {
        Log.d(sqw, "ON_STOP")

        super.onStop()
    }


    override fun onDestroy() {
        Log.d(sqw, "ON_DESTROY")
        scope.cancel()
        if (isStoreBound) {
            unbindService(boundMyStoreServiceConnection)
        }
        super.onDestroy()
    }



    private suspend fun readNewMessage() : List<OnServerJSONMessage> {
        val lastId = if (lastPos == -1) {
            0
        } else {
            listAllMessage.last().id
        }
        val response = withContext(Dispatchers.IO) {
            try {
                serversWorkService.getListNewMessage(lastId, 70).execute()
            } catch (e : Exception) {
                localeExceptions(e, "(read_new_messages)")
                null
            }
        } ?: return emptyList()

        localeHTTPProblems(response, "(read_new_messages)")
        return response.body()!!
    }


    private suspend fun loadMiniImage(message : Message) {
        if (message.text_message == null || message.path == "") {
            return
        }

        val response = withContext(Dispatchers.IO) {
            try {
                serversWorkService.loadMiniImage(message.path).execute()
            } catch (e : Exception) {
                if (e is CancellationException) {
                    message.text_message = "...DownloadImage..."
                    return@withContext null
                }
                message.text_message = "ERROR"
                Log.d(sqw, message.id.toString())
                messagesAdapter.updateElement(message.id)
                Log.d(sqw, message.id.toString())
                localeExceptions(e, "(load_new_mini_image ${message.id})")
                null
            }
        } ?: return

        localeHTTPProblems(response, "(load_new_mini_image ${message.id})")
        message.image = withContext(Dispatchers.IO) {
            val bitmap = BitmapFactory.decodeStream(response.body()?.byteStream())
            if (response.body() != null) {
                response.body()!!.close()
            }
            bitmap
        }
        message.text_message = null
        messagesAdapter.updateElement(message.id)

        withContext(Dispatchers.IO) {
            saveImageToPrivateExternalStorage(message)
            val db = MyApp.staticInstance?.myDatabase!!
            db.messageDao()?.update(message)
        }
    }

    private suspend fun readAllNewMessage() {
        while (true) {
            listAllMessage.addAll(readNewMessage().toList().map {
                serverMessageToRecyclerViewMessage(it)
            })
            if (lastPos != -1 && lastPos == listAllMessage.size - 1) {
                break
            } else {
                messagesAdapter.add(lastPos)
            }

            lastPos = if (listAllMessage.isNotEmpty()) {
                listAllMessage.size - 1
            } else {
                break
            }
        }
        loadToDB()
        val tmp = globalLastPos
        globalLastPos = lastPos
        for (i in (tmp + 1) until listAllMessage.size) {
            loadMiniImage(listAllMessage[i])
        }
    }

    private suspend fun loadToDB() {
        Log.d(sqw, "Load_To_DB")  //
        withContext(Dispatchers.IO) {
            var ind = 0
            val db = MyApp.staticInstance?.myDatabase!!

            val lastId = db.messageDao()?.getMaxID()

            if (lastId != null) {
                for (i in (listAllMessage.size - 1) downTo 0) {
                    if (lastId.id == listAllMessage[i].id) {
                        ind = i + 1
                        break
                    }
                }
            }



            for (i in (ind) until listAllMessage.size) {
                //println("${listAllMessage[i].id} : $i")
                val message = listAllMessage[i]
                saveImageToPrivateExternalStorage(message)
                db.messageDao()?.insert(message)
            }
        }

    }

    private fun saveImageToPrivateExternalStorage(message: Message) {
        if (message.image == null) {
            return
        }
        val absPathToImage =
            pathToPrivateStorage + File.separator +
                    System.currentTimeMillis() + "image.png"
        Log.d(sqw, "loadToDB : $absPathToImage")
        File(absPathToImage).outputStream().use {
            message.image?.compress(Bitmap.CompressFormat.PNG, 100, it)
        }
        message.absolutelyPath = absPathToImage
    }

    private fun drawRecyclerView() {
        listAllMessage = myStoreService?.currentListMessage!!

        //attached to list in the service
        messagesAdapter = MessageAdapter(listAllMessage) {
            val imageIntent = Intent(this@MainActivity, OpenImageActivity::class.java)
            imageIntent.putExtra("path", it.path)
            startActivity(imageIntent)
            //bigPicture - OK
        }

        listMessage.apply {
            layoutManager = viewManager
            adapter = messagesAdapter
        }

        drawRecyclerViewJob = scope.launch {
            if (flagFirstStartActivity) { //первый запуск
                flagFirstStartActivity = false
                if (!restoreAllAtDB()) {
                    //читаем с сервера в самый первый запуск на у-ве
                    readAllNewMessage()

                } else {
                    messagesAdapter.add(-1)
                }
            } else { // иначе все уже загрузилось из StoreService
                lastPos = listAllMessage.size - 1
            }
            for (i  in 0 until listAllMessage.size) {
                loadMiniImage(listAllMessage[i]) //проверяем все картинки на всякий случай
            }
        }
    }

    private suspend fun restoreAllAtDB() : Boolean {
        val db = MyApp.staticInstance?.myDatabase!!
        val dbDataList = withContext(Dispatchers.IO) {
            db.messageDao()?.getAll()
        }

        if (dbDataList == null || dbDataList.isEmpty()) { // базы нет, это - первый запуск
            return false
        }

        Log.d(sqw, "RESTORE_WITHOUT_INTERNET (SIZE_DB : ${dbDataList.size})")
        withContext(Dispatchers.IO) {
            for (message in dbDataList) {
                if (message.absolutelyPath != "") {
                    File(message.absolutelyPath).inputStream().use {
                        message.image = BitmapFactory.decodeStream(it)
                    }
                }
            }
        }
        listAllMessage.addAll(dbDataList)
        lastPos = listAllMessage.size - 1
        return true
    }


    private fun <T> localeHTTPProblems(response : retrofit2.Response<T>, area : String) {
        Log.d(sqw, response.code().toString() + " " + response.message().toString())

        if (!response.isSuccessful) {
            when (response.code().toString().first()) {
                '1', '3' -> {
                    Log.d(sqw, "(1xx, 3xx) REDIRECTED $area")
                }
                '4' -> {
                    when(response.code()) {
                        404 -> {
                            logException(area, "NOT_FOUND_SERVER", "СЕРВЕР НЕ НАЙДЕН (404) !!!")
                        }
                        409 -> {
                            logException(area, "CONFLICT_WITH_SERVER", "Конфликт с сервером (409) !!!")
                        }

                        411 -> {
                            logException(area, "Length Required", "Не указан Content-Length (411) !!!")
                        }

                        413 -> {
                            logException(area, "Request Entity Too Large", "Слишком большой запрос (413) !!!")
                        }

                        415 -> {
                            logException(area, "Unsupported Media Type", "Кривой Content-Type у всего сообщения (415) !!!")
                        }

                        422 -> {
                            logException(area, "Unprocessable Entity", "Неполадка с переданными данными (422) !!!")
                        }

                        else -> {
                            logException(area, "Такого кода быть совсем не должно...", "4xx Такого кода быть не должно...")
                        }
                    }
                }
                '5' -> {
                    logException(area, "(1xx, 3xx) REDIRECTED", "5xx Проблема с сервером не на нашей стороне")
                }
                else -> { //сюда не попадем(
                    logException(area, "Происходит что-то странное...", "Происходит что-то странное...")
                }
            }
        } else {
            Log.d(sqw, "GOOD okhttp3_connection - response : 2xx=OK $area")
        }
    }


    private fun localeExceptions(e : Exception, area : String) {
        Log.d(sqw, e.message.toString() + " " + area)
        when (e) {
            is RuntimeException -> {
                logException(area, "RUN_TIME_EXCEPTION", "RUN_TIME_EXCEPTION!!! МЫ СУПЕР_ПРОИГРАЛИ!!!")
            }
            is IOException -> {
                logException(area, "IOE_EXCEPTION", "Видимо что-то с глобальным интернетом...")
            }
            else -> {
                logException(area, "..._EXCEPTION", "Видимо это МЕГА_ПРОИГРЫШ")
            }
        }
        e.printStackTrace()
    }

    private fun logException(area: String = "", firstMes : String = "", secondMes : String = "") {
        Log.d(sqw, "$firstMes $area")
        Toast.makeText(
            this@MainActivity,
            "$secondMes $area",
            Toast.LENGTH_SHORT
        ).show()
    }
}