package hw.hw71.chat

import android.os.Bundle
import androidx.fragment.app.Fragment
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.app.Activity
import android.content.ComponentName
import android.content.Intent
import android.content.ServiceConnection
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.ImageDecoder
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.util.Log
import android.widget.Button
import android.widget.EditText
import android.widget.Toast
import androidx.activity.result.ActivityResultLauncher
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import hw.hw71.*
import hw.hw71.data_classes.OnServerJSONMessage
import hw.hw71.data_classes.ToServerJSONMessage
import hw.hw71.services.MyApp
import hw.hw71.services.MyStoreService
import hw.hw71.services.ServersWorkService
import hw.hw71.services.serverMessageToRecyclerViewMessage
import kotlinx.coroutines.*
import okhttp3.MediaType
import okhttp3.MultipartBody
import okhttp3.RequestBody
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.IOException

class ChatFragment : Fragment() {

    private var type = false
    private lateinit var nameChannel : String

    private var drawRecyclerViewJob: Job? = null
    private lateinit var pathToPrivateStorage: String
    private val sqw = "CHAT_FRAGMENT"

    private var MY_KEY_ONE = "RESTORE_BOOLEAN"
    private var flagFirstStartActivity = true
    private var flagOnCreate = false

    private var globalLastPos = -1
    private var lastPos = -1
    private lateinit var listAllMessage : ArrayList<Message>
    private lateinit var mapIDInPosition : HashMap<Int, Int>


    private lateinit var viewManager : LinearLayoutManager
    private lateinit var listMessage : RecyclerView
    private lateinit var messagesAdapter : MessageAdapter
    private lateinit var editText : EditText
    private lateinit var swipeRefreshLayout : SwipeRefreshLayout
    private lateinit var runnable : java.lang.Runnable
    private lateinit var handler : Handler
    private lateinit var resultLauncherOfSendingImage : ActivityResultLauncher<Intent>

    private var scope : CoroutineScope = CoroutineScope(Dispatchers.Main)
    private lateinit var serversWorkService : ServersWorkService



    var myStoreService : MyStoreService? = null
    var isStoreBound = false
    private val boundMyStoreServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            Log.d(sqw, "SERVICE_STORE_CONNECT")
            val binderBridge: MyStoreService.MyStoreBinder = service as MyStoreService.MyStoreBinder
            myStoreService = binderBridge.getMyService()
            isStoreBound = true
            if (myStoreService?.curNameChannel != null && myStoreService?.curNameChannel != nameChannel) {
                myStoreService?.currentListMessage = ArrayList()
                myStoreService?.mapIDInPosition = HashMap()
                flagFirstStartActivity = true
            } else if (myStoreService?.curNameChannel != null) {
                flagFirstStartActivity = false
            }
            myStoreService?.curNameChannel = nameChannel
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
        //setContentView(R.layout.main_chat_activity)
        if (savedInstanceState != null) {
            Log.d(sqw, "НЕ ПЕРВЫЙ ЗАПУСК!!!")
            flagFirstStartActivity = savedInstanceState.getBoolean(MY_KEY_ONE, true)
        }

        pathToPrivateStorage = activity?.getExternalFilesDir(null)?.path + File.separator + "myImages"
        val imagesDir = File(pathToPrivateStorage)
        if (!imagesDir.exists()) {
            imagesDir.mkdir()
        }
        flagOnCreate = true

        resultLauncherOfSendingImage = createSendingImageActivityResultLauncher()
        //createActivity()
    }


    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        Log.d(sqw, "ON_VIEW_CREATED")
        super.onViewCreated(view, savedInstanceState)
        val context = view.context
        viewManager =  LinearLayoutManager(context)
        swipeRefreshLayout = view.findViewById(R.id.swipeRefreshLayot)
        listMessage = view.findViewById(R.id.list_messages)
        viewManager.reverseLayout = true
        viewManager.stackFromEnd = true

        serversWorkService = MyApp.staticInstance?.serversWorkService!!

        createSendingMessage(view)
        createRefreshingMessages()
        createSendingImage(view)

    }


    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?
    ): View {
        Log.d(sqw, "ON_CREATE_VIEW")
        return inflater.inflate(R.layout.chat_fragment, container, false)
    }


    private fun createSendingMessage(view: View) {
        val buttonSendText = view.findViewById<Button>(R.id.button_for_send_text)
        editText = view.findViewById(R.id.new_message)
        buttonSendText.setOnClickListener {
            scope.launch {
                val response = withContext(Dispatchers.IO) {
                    try {
                        serversWorkService.sendTextMessage(
                            ToServerJSONMessage(
                                "Moahim2",
                                nameChannel,
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


     private fun createSendingImageActivityResultLauncher() : ActivityResultLauncher<Intent> {
         return registerForActivityResult(
             ActivityResultContracts.StartActivityForResult()
         )
         { res ->
             if (res.resultCode == Activity.RESULT_OK) {
                 val uriImage = res.data?.data
                 if (uriImage == null) {
                     Toast.makeText(
                         activity,
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
                                     requireActivity().contentResolver,
                                     uriImage
                                 )
                             )
                             var imageArray: ByteArray?
                             ByteArrayOutputStream().use {
                                 bitmap.compress(Bitmap.CompressFormat.JPEG, 90, it)
                                 imageArray = it.toByteArray()
                             }
                             val imageFile = File(
                                 activity?.baseContext?.cacheDir,
                                 "image${System.currentTimeMillis()}.jpeg"
                             )
                             imageFile.outputStream().use {
                                 it.write(imageArray)
                                 it.flush()
                             }

                             val json = MyApp.staticInstance?.moshiWriterSendingMessage?.toJson(
                                 ToServerJSONMessage(
                                     "Moahim2",
                                     nameChannel,
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
     }

    private fun createSendingImage(view: View) {
        val buttonSendImage = view.findViewById<Button>(R.id.button_for_send_image)
        buttonSendImage.setOnClickListener {
            val imagePick = Intent(Intent.ACTION_PICK)
            imagePick.type = "image/*"

            resultLauncherOfSendingImage.launch(imagePick)
        }
    }


    override fun onStart() {
            Log.d(sqw, "ON_START")
        super.onStart()

        nameChannel = this.requireArguments().getString("NAME")!!
        if (!nameChannel.contains("@")) {
            type = true
        }
        val starterMyStoreService = Intent(activity, MyStoreService::class.java)
        activity?.startService(starterMyStoreService)
        activity?.bindService(starterMyStoreService, boundMyStoreServiceConnection,
            AppCompatActivity.BIND_AUTO_CREATE
        )
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
            activity?.unbindService(boundMyStoreServiceConnection)
        }
        super.onDestroy()
    }

    override fun onDetach() {
        Log.d(sqw, "ON_DETACH")
        super.onDetach()
    }


    private suspend fun readNewMessage() : List<OnServerJSONMessage> {
        val lastId = if (lastPos == -1) {
            0
        } else {
            listAllMessage.last().id
        }
        val response = withContext(Dispatchers.IO) {
            try {
                serversWorkService.getListNewMessage(nameChannel ,lastId, 70).execute()
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
                mapIDInPosition[message.id]?.let { messagesAdapter.updateElement(it) }
                Log.d(sqw, message.id.toString())
                localeExceptions(e, "(load_new_mini_image ${message.id})")
                null
            }
        } ?: return

        ////TODO(вроде доделал обновление...............)
        localeHTTPProblems(response, "(load_new_mini_image ${message.id})")
        message.image = withContext(Dispatchers.IO) {
            val bitmap = BitmapFactory.decodeStream(response.body()?.byteStream())
            if (response.body() != null) {
                response.body()!!.close()
            }
            bitmap
        }
        message.text_message = null
        mapIDInPosition[message.id]?.let { messagesAdapter.updateElement(it) }

        withContext(Dispatchers.IO) {
            saveImageToPrivateExternalStorage(message)
            val db = MyApp.staticInstance?.myDatabase!!
            db.messageDao()?.update(message)
        }
    }

    private suspend fun readAllNewMessage() {
        if (!type) {
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
        } else {
            val response2 = withContext(Dispatchers.IO) {
                val ans = ArrayList<Message>()
                var count = lastPos
                println(count)
                println("FFFFFFFFFFFFFFFFFF")
                while (true) {
                    val list = try {
                        MyApp.staticInstance?.serversWorkService?.getInboxMsgForMe(
                            count, 70
                        )?.execute()?.body()!!
                    } catch (e : Exception) {
                        e.printStackTrace()
                        Toast.makeText(activity,
                            "НЕТ ИНТЕРНЕТА, мы проиграли в чаты", Toast.LENGTH_SHORT).show()
                        arrayListOf()
                    }
                    if (list.isEmpty()) {
                        break
                    }
                    count = list.last().id
                    for (i in list) {
                        if (i.to == "Moahim2") {
                            if (i.from == nameChannel) {
                                ans.add(serverMessageToRecyclerViewMessage(i))
                            }
                        }
                        if (i.to == nameChannel) {
                            if (i.from == "Moahim2") {
                                ans.add(serverMessageToRecyclerViewMessage(i))
                            }
                        }
                    }
                }
                lastPos = count
                ans
            }

            val x = listAllMessage.size
            println("res : ${response2.size}")
            listAllMessage.addAll(response2)
            messagesAdapter.add(x - 1)
        }
        loadToDB()
        val tmp = globalLastPos
        globalLastPos = lastPos
        if (type) {
            globalLastPos = listAllMessage.size - 1
        }
        for (i in (tmp + 1) until listAllMessage.size) {
            mapIDInPosition[listAllMessage[i].id] = i
            loadMiniImage(listAllMessage[i])
        }
    }

    private suspend fun loadToDB() {
        Log.d(sqw, "Load_To_DB")  //
        withContext(Dispatchers.IO) {
            var ind = 0
            val db = MyApp.staticInstance?.myDatabase!!
            val msgDao = db.messageDao()
            val lastId = msgDao?.getMaxID()

            if (lastId != null) {
                for (i in (listAllMessage.size - 1) downTo 0) {
                    if (lastId.id == listAllMessage[i].id) {
                        ind = i + 1
                        break
                    }
                }
            }



            for (i in (ind) until listAllMessage.size) {
                val message = listAllMessage[i]
                val tmpM = msgDao?.getOnID(message.id)
                if (tmpM == null || tmpM.id != message.id) {
                    saveImageToPrivateExternalStorage(message)
                    db.messageDao()?.insert(message)
                }
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
        Log.d(sqw, "sizeListService : ${myStoreService?.currentListMessage?.size}")
        Log.d(sqw, "FIRST_START_CHANNEL_${nameChannel} : $flagFirstStartActivity")

        listAllMessage = myStoreService?.currentListMessage!!
        mapIDInPosition = myStoreService?.mapIDInPosition!!

        //attached to list in the service
        messagesAdapter = MessageAdapter(listAllMessage) {
            val imageIntent = Intent(activity, OpenImageActivity::class.java)
            imageIntent.putExtra("path", it.path)
            startActivity(imageIntent)
            //bigPicture - OK
        }

        listMessage.apply {
            layoutManager = viewManager
            adapter = messagesAdapter
        }
        //////////////////////////////////////
        scope = CoroutineScope(Dispatchers.Main)

        drawRecyclerViewJob = scope.launch {
            if (flagFirstStartActivity) { //первый запуск
                flagFirstStartActivity = false
                if (type || !restoreAllAtDB()) {
                    //читаем с сервера в самый первый запуск на у-ве
                    readAllNewMessage()

                } else {
                    messagesAdapter.add(-1)
                }
            } else { // иначе все уже загрузилось из StoreService
                lastPos = listAllMessage.size - 1
                if (type) {
                    lastPos = listAllMessage.last().id
                }
            }
            println("DRAW")
            for (i  in 0 until listAllMessage.size) {
                //println(i)
                loadMiniImage(listAllMessage[i]) //проверяем все картинки на всякий случай
            }
        }
    }

    private suspend fun restoreAllAtDB() : Boolean {
        val db = MyApp.staticInstance?.myDatabase!!
        val dbDataList = withContext(Dispatchers.IO) {
            db.messageDao()?.getAll(nameChannel)
        }
        Log.d(sqw, "TRYRestoreAtDB : ${dbDataList?.size})")
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
        activity?.runOnUiThread(Runnable {
            run {
                Toast.makeText(
                    activity,
                    "$secondMes $area",
                    Toast.LENGTH_SHORT
                ).show()
            }
        })
    }

    companion object {
        private const val KEY_NAME = "NAME"

        fun create(name : String) : ChatFragment {
            return ChatFragment().apply {
                arguments = Bundle(1).apply {
                    putString(KEY_NAME, name)
                }
            }
        }
    }

//    companion object {
//        /**
//         * Use this factory method to create a new instance of
//         * this fragment using the provided parameters.
//         *
//         * @param param1 Parameter 1.
//         * @param param2 Parameter 2.
//         * @return A new instance of fragment ChatFragment.
//         */
//        // TODO: Rename and change types and number of parameters
//        @JvmStatic
//        fun newInstance(param1: String, param2: String) =
//            ChatFragment().apply {
//                arguments = Bundle().apply {
//                    putString(ARG_PARAM1, param1)
//                    putString(ARG_PARAM2, param2)
//                }
//            }
//    }
}