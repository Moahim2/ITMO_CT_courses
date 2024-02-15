package hw.hw4

import android.app.Activity
import android.content.*
import android.os.*
import android.util.Log
import android.widget.Button
import android.widget.EditText
import androidx.activity.result.contract.ActivityResultContracts
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import java.util.concurrent.CopyOnWriteArrayList

class MainActivity : AppCompatActivity() {



    private var logMA = "LOGS_HW4"
    private var MY_KEY_ONE = "RESTORE_BOOLEAN"

    private var flag : Boolean = false
    private var isCatch = false


    private val viewManager = LinearLayoutManager(this)
    private lateinit var listMessage : RecyclerView
    private lateinit var messagesAdapter : MessageAdapter
    private lateinit var editText : EditText
    private lateinit var swipeRefreshLayout : SwipeRefreshLayout
    private lateinit var runnable : Runnable
    private lateinit var handler : Handler

    private lateinit var exchangeIntent : Intent
//
    var myIntentService : MyIntentService? = null
    var isIntentBound = false
    private val boundIntentServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            val binderBridge: MyIntentService.MyBinder = service as MyIntentService.MyBinder
            myIntentService = binderBridge.getMyService()
            Log.d(logMA, "INTENT_SERVICE_CONNECT")
            if (flag) {
                myIntentService?.flagBlockLoadImage = true
            }
            isIntentBound = true
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d(logMA, "INTENT_SERVICE_DISCONNECT")
            isIntentBound = false
            myIntentService = null
        }
    }
///////////////////

    var myStoreService : MyStoreService? = null
    var isStoreBound = false
    private val boundMyStoreServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            Log.d(logMA, "SERVICE_STORE_CONNECT")
            val binderBridge: MyStoreService.MyStoreBinder = service as MyStoreService.MyStoreBinder
            myStoreService = binderBridge.getMyService()
            isStoreBound = true
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d(logMA, "SERVICE_STORE_DISCONNECT")
            isStoreBound = false
            myIntentService = null
        }
    }


    private fun restoreAtDBOrServer() {
        exchangeIntent.putExtra("type", "restoreAllAtDB")
        startService(exchangeIntent)
    }

    private fun loadToDB() { //
        exchangeIntent.putExtra("type", "loadToDB")
        startService(exchangeIntent)
    }

//HW5SendImage///////////////////////////

    private var resultLauncher = registerForActivityResult(ActivityResultContracts.StartActivityForResult()) {
        if (it.resultCode == Activity.RESULT_OK) {
            val sendImageIntent = Intent(this, MyIntentService::class.java)
            sendImageIntent.putExtra("type", "sendImage")
            sendImageIntent.putExtra("val", it.data?.data.toString())
            startService(sendImageIntent)
        }
    }
/////////////////
    override fun onStart() {
        super.onStart()
        //Start AllService and RestoreOrLoad

        exchangeIntent = Intent(this, MyIntentService::class.java)

        val starterMyIntentService = Intent(this, MyIntentService::class.java)
        val starterMyStoreService = Intent(this, MyStoreService::class.java)

        startService(starterMyStoreService)
        bindService(starterMyStoreService, boundMyStoreServiceConnection, BIND_AUTO_CREATE)

        startService(starterMyIntentService)
        bindService(starterMyIntentService, boundIntentServiceConnection, BIND_AUTO_CREATE)

        if (!flag) {// разделение
            restoreAtDBOrServer()
        } else {
            exchangeIntent.putExtra("type", "restoreAll")
            startService(exchangeIntent)
        }
    }



    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        Log.d(logMA, "ON_CREATE_MAIN_ACTIVITY")
        if (savedInstanceState != null) {
            flag = savedInstanceState.getBoolean(MY_KEY_ONE, false)
        }
        createActivity()
    }

    private fun createActivity() {
        swipeRefreshLayout = findViewById(R.id.swipeRefreshLayot)
        listMessage = findViewById(R.id.list_messages)
        viewManager.reverseLayout = true
        viewManager.stackFromEnd = true

//приемник
        val allIntentsFilter = IntentFilter()
        allIntentsFilter.addAction("loadAll")
        allIntentsFilter.addAction("update")
            allIntentsFilter.addAction("miniImage")
        allIntentsFilter.addAction("bigImage")
        allIntentsFilter.addAction("restore")
        allIntentsFilter.addAction("restoreAtDB")
        registerReceiver( MyReceiver { list ->
            messagesAdapter = MessageAdapter(list) {
                startService(
                    exchangeIntent.putExtra("type", "loadBigImage").
                    putExtra("val", it.path))
            }
            listMessage.apply {
                layoutManager = viewManager
                adapter = messagesAdapter
            }

            isCatch = true
            for (mes in list) {
                findMiniImage(mes)
            }
        }, allIntentsFilter)
///////
//////Обновление онлайн (кнопка + прокручивание)
        val buttonSendText = findViewById<Button>(R.id.button_for_send_text)
        editText = findViewById(R.id.new_message)
        buttonSendText.setOnClickListener {
            startService(exchangeIntent.putExtra("type", "send").putExtra("val",
                editText.text.toString()))
            editText.setText("")
        }


        handler = Handler(Looper.getMainLooper())
        swipeRefreshLayout.setOnRefreshListener {
            runnable = Runnable {
                startService(exchangeIntent.putExtra("type", "loadNew"))
                swipeRefreshLayout.isRefreshing = false
            }
            handler.postDelayed(runnable, 1.toLong())
        }

//////////HW5
        val buttonSendImage = findViewById<Button>(R.id.button_for_send_image)
        buttonSendImage.setOnClickListener {
            val imagePick = Intent(Intent.ACTION_PICK)
            imagePick.type = "image/*"
            resultLauncher.launch(imagePick)
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        myStoreService?.saveAllList(myIntentService!!.currentListMessage)
        flag = true
        outState.putBoolean(MY_KEY_ONE, true)
        Log.d(logMA, "ON_SAVE_INSTANCE_STATE")
    }


    override fun onStop() {
        super.onStop()
        if (isIntentBound) {
            unbindService(boundIntentServiceConnection)
        }
        Log.d(logMA, "ON_STOP_MAIN_ACTIVITY")
    }

    override fun onPause() {
        super.onPause()
        Log.d(logMA, "$isIntentBound : ON_PAUSE_MAIN_ACTIVITY")
        loadToDB()
    }

    override fun onDestroy() {
        super.onDestroy()
        Log.d(logMA, "$isIntentBound : ON_DESTROY_MAIN_ACTIVITY")
        if (isStoreBound) {
            unbindService(boundMyStoreServiceConnection)
        }
    }

    inner class MyReceiver(private val fillRecyclerView : (CopyOnWriteArrayList<Message>) -> Unit) : BroadcastReceiver() {

        override fun onReceive(context: Context?, intent: Intent?) {

            when(intent?.action.toString()) {
                "loadAll" -> {
                    val list = myIntentService!!.currentListMessage
                    fillRecyclerView.invoke(list)
                }

                "update" -> {
                    if (isCatch) {
                        val lastPos = intent?.extras?.getInt("val", 0)!!
                        messagesAdapter.add(lastPos)
                        for (i in (lastPos + 1) until myIntentService!!.currentListMessage.size) {
                            findMiniImage(myIntentService!!.currentListMessage[i])
                        }
                    }
                }

                "miniImage" -> {
                    if (isCatch) {
                        messagesAdapter.updateElement(intent?.extras?.getInt("id")!!)
                    }
                }

                "bigImage" -> {
                    if (myIntentService?.curBigBitMap != null) {
                        val imageIntent = Intent(this@MainActivity, OpenImageActivity::class.java)
                        startActivity(imageIntent)
                    }
                }

                "restore" -> { //AtStoreService
                    isCatch = true
                    Log.d(logMA, "restoreAtStoreService")
                    val list = CopyOnWriteArrayList(myStoreService!!.currentStoreListMessage)
                    messagesAdapter = MessageAdapter(list) {
                        startService(
                            exchangeIntent.putExtra("type", "loadBigImage").
                            putExtra("val", it.path))
                    }
                    listMessage.apply {
                        layoutManager = viewManager
                        adapter = messagesAdapter
                    }

                    myIntentService?.currentListMessage = list
                    myIntentService?.lastID = myIntentService?.currentListMessage?.last()!!.id

                    myIntentService?.flagBlockLoadImage = false
                    for (mes in myStoreService!!.currentStoreListMessage) {
                        findMiniImage(mes)
                    }
                }

                //HW5//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                "restoreAtDB" -> {
                    if (!intent?.extras?.getBoolean("flag", false)!!) {
                        Log.d(logMA, "LOAD_OF_INTERNET")
                        exchangeIntent.putExtra("type", "readAll")
                        startService(exchangeIntent)
                    } else {
                        Log.d(logMA, "RESTORE_AT_DB : WITHOUT_INTERNET")
                        val list = myIntentService!!.currentListMessage
                        fillRecyclerView.invoke(list)
                    }
                }
            }
        }
    }


    fun findMiniImage(message: Message) {
        if (message.text_message == null || message.path == "") {
            return
        }
        startService(
            exchangeIntent.putExtra("type", "loadMiniImage").
            putExtra("val", message.path).putExtra("id", message.id)
        )
    }
}