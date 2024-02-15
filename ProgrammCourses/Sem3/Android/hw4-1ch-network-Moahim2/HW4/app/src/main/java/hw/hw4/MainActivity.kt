package hw.hw4

import android.content.*
import android.os.*
import android.util.Log
import androidx.appcompat.app.AppCompatActivity
import android.widget.Button
import android.widget.EditText
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout

class MainActivity : AppCompatActivity() {


    private var LOG = "LOGS_HW4"
    private var MY_KEY_ONE = "RESTORE_BOOLEAN"
    private var flag : Boolean = false

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
            Log.d(LOG, "OPEN_SERVICE_CONNECT")
            isIntentBound = true
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d(LOG, "SERVICE_DISCONNECT")
            isIntentBound = false
            myIntentService = null
        }
    }
///////////////////

    var myStoreService : MyStoreService? = null
    var isStoreBound = false
    private val boundMyStoreServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            Log.d(LOG, "SERVICE_STORE_CONNECT")
            val binderBridge: MyStoreService.MyStoreBinder = service as MyStoreService.MyStoreBinder
            myStoreService = binderBridge.getMyService()
            isStoreBound = true
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d(LOG, "SERVICE_STORE_DISCONNECT")
            isStoreBound = false
            myIntentService = null
        }
    }



    override fun onStart() {
        super.onStart()
        //Start IntentService
        exchangeIntent = Intent(this, MyIntentService::class.java)

        val starterMyIntentService = Intent(this, MyIntentService::class.java)
        val starterMyStoreService = Intent(this, MyStoreService::class.java)

        startService(starterMyStoreService)
        bindService(starterMyStoreService, boundMyStoreServiceConnection, BIND_AUTO_CREATE)

        startService(starterMyIntentService)
        bindService(starterMyIntentService, boundIntentServiceConnection, BIND_AUTO_CREATE)

        if (!flag) {
            exchangeIntent.putExtra("type", "readAll")
            startService(exchangeIntent)
        } else {
            exchangeIntent.putExtra("type", "restoreAll")
            startService(exchangeIntent)
        }
    }





    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        if (savedInstanceState != null) {
            flag = savedInstanceState.getBoolean(MY_KEY_ONE, false)
        }
        swipeRefreshLayout = findViewById(R.id.swipeRefreshLayot)
        listMessage = findViewById(R.id.list_messages)


        val allIntentsFilter = IntentFilter()
        allIntentsFilter.addAction("loadAll")
        allIntentsFilter.addAction("update")
        allIntentsFilter.addAction("miniImage")
        allIntentsFilter.addAction("bigImage")
        allIntentsFilter.addAction("restore")
        registerReceiver(MyReceiver { list ->
            messagesAdapter = MessageAdapter(list) {
                startService(
                    exchangeIntent.putExtra("type", "loadBigImage").
                    putExtra("val", it.path))
            }
            for (mes in list) {
                findMiniImage(mes)
            }
            listMessage.apply {
                layoutManager = viewManager
                adapter = messagesAdapter
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

    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        myStoreService?.saveAllList(myIntentService!!.currentListMessage)
        outState.putBoolean(MY_KEY_ONE, true)
    }

    override fun onPause() {
        super.onPause()
        Log.d(LOG, "$isIntentBound : ONPAUSE")
    }

    override fun onDestroy() {
        super.onDestroy()
        Log.d(LOG, "$isIntentBound : ONDestroy")
        if (isIntentBound) {
            unbindService(boundIntentServiceConnection)
        }
        if (isStoreBound) {
            unbindService(boundMyStoreServiceConnection)
        }
    }


    inner class MyReceiver(private val fillRecyclerView : (ArrayList<Message>) -> Unit) : BroadcastReceiver() {

        override fun onReceive(context: Context?, intent: Intent?) {
            when(intent?.action.toString()) {
                "loadAll" -> {
                    val list = ArrayList(myIntentService!!.currentListMessage.map { it.copy() })
                    fillRecyclerView.invoke(list)
                }

                "update" -> {
                    for (message in myIntentService!!.listNewMessages) {
                        messagesAdapter.add(message)
                        findMiniImage(message)
                    }
                }

                "miniImage" -> {
                    if (myIntentService?.curMiniBitMap != null) {
                        messagesAdapter.updateElement(
                            intent?.extras?.getInt("id")!!,
                            myIntentService?.curMiniBitMap!!.copy(myIntentService?.curMiniBitMap!!.config, true))
                    }
                }

                "bigImage" -> {
                    if (myIntentService?.curBigByteArray != null) {
                        val imageIntent = Intent(this@MainActivity, OpenImageActivity::class.java)
                        imageIntent.putExtra("imageBytesArray", myIntentService?.curBigByteArray)
                        startActivity(imageIntent)
                    }
                }

                "restore" -> {
                    messagesAdapter = MessageAdapter(
                        myStoreService!!.copyAllList(myStoreService!!.currentStoreListMessage)
                    ) {
                        startService(
                            exchangeIntent.putExtra("type", "loadBigImage").
                            putExtra("val", it.path))
                    }
                    listMessage.apply {
                        layoutManager = viewManager
                        adapter = messagesAdapter
                    }
                    myIntentService?.currentListMessage =
                        myStoreService!!.copyAllList(myStoreService!!.currentStoreListMessage)
                    myIntentService?.lastID = myIntentService?.currentListMessage?.last()!!.id
                }
            }
        }
    }

    fun findMiniImage(message: Message) {
        if (message.path == "") {
            return
        }
        startService(
            exchangeIntent.putExtra("type", "loadMiniImage").
            putExtra("val", message.path).putExtra("id", message.id)
        )
    }

}