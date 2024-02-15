package hw.hw71.listChats

import android.content.Context
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.EditText
import android.widget.Toast
import androidx.fragment.app.Fragment
import androidx.lifecycle.lifecycleScope
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout
import hw.hw71.services.MyApp
import hw.hw71.R
import hw.hw71.data_classes.ToServerJSONMessage
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext


class ListChatFragments : Fragment() {

    private val KEY_CHANNELS = "KEY_CHANNELS"
    private lateinit var recyclerView: RecyclerView
    private lateinit var adapter: ChatAdapter
    private var listAllChannels : ArrayList<String> = arrayListOf()

    private lateinit var editText : EditText
    private lateinit var swipeRefreshLayout : SwipeRefreshLayout
    private lateinit var runnable : Runnable
    private lateinit var handler : Handler

    override fun onAttach(context: Context) {
        Log.d(KEY_LOG, "ON_ATTACH")
        super.onAttach(context)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.d(KEY_LOG, "ON_CREATE")
        adapter = ChatAdapter(listAllChannels) { view, nameChannel ->
            (context as OnChatClick).onClickChat(view, nameChannel)
        }

        if (savedInstanceState != null) {
            Log.d(KEY_LOG, "ON_CREATE_RESTORE")
            listAllChannels.addAll(savedInstanceState.getStringArrayList(KEY_CHANNELS) as ArrayList<String>)
            adapter.notifyItemRangeInserted(0, listAllChannels.size)
            return
        }


        lifecycleScope.launch(Dispatchers.Main) {
            val response = withContext(Dispatchers.IO) {
                readAllChats()
            }
            Log.d(KEY_LOG, response.toString())

            listAllChannels.addAll(response)
            adapter.notifyItemRangeInserted(0, response.size)
            val response2 = withContext(Dispatchers.IO) {
                readMyMsg()
            }
            val tmp = listAllChannels.size
            listAllChannels.addAll(response2)
            adapter.notifyItemRangeInserted(tmp, listAllChannels.size - tmp)
        }
    }

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        return inflater.inflate(R.layout.list_chat_fragments, container, false)
    }


    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)

        val context = view.context
        swipeRefreshLayout = view.findViewById(R.id.swipeRefreshLayotListChat)!!
        editText = view.findViewById(R.id.new_chat_message)
        recyclerView = view.findViewById(R.id.chats_list_recycler)
        recyclerView.adapter = adapter
        recyclerView.layoutManager = LinearLayoutManager(context, LinearLayoutManager.VERTICAL, false)

        val button = view.findViewById<Button>(R.id.button_for_create_chat)
        button.setOnClickListener {
            lifecycleScope.launch(Dispatchers.Main) {
                val str = editText.text
                editText.setText("")
                if (str.count { return@count it.isWhitespace() } != 1 ) {
                    Toast.makeText(activity, "Должен быть ровно один пробел", Toast.LENGTH_SHORT).show()
                    return@launch
                }

                val nameAndMessage = str.split(" ")
                val where = if (nameAndMessage[0].first() == '%') {
                    nameAndMessage[0].substring(1)
                } else {
                    nameAndMessage[0] + "@channel"
                }
                withContext(Dispatchers.IO) {
                    try {
                        MyApp.staticInstance?.serversWorkService?.sendTextMessage(
                            ToServerJSONMessage(
                                "Moahim2",
                                where,
                                mapOf(Pair("Text", mapOf(Pair("text", nameAndMessage[1]))))
                            )
                        )?.execute()
                    } catch (e : Exception) {
                        e.printStackTrace()
                        Toast.makeText(activity,
                            "НЕТ ИНТЕРНЕТА, мы проиграли в чаты", Toast.LENGTH_SHORT).show()
                    }
                }
            }
        }

        handler = Handler(Looper.getMainLooper())
        swipeRefreshLayout.setOnRefreshListener {
            runnable = kotlinx.coroutines.Runnable {
                swipeRefreshLayout.isRefreshing = false

               lifecycleScope.launch(Dispatchers.Main) {
                   val response = withContext(Dispatchers.IO) {
                       readAllChats()
                   }
                   adapter.notifyItemRangeRemoved(0, listAllChannels.size)
                   listAllChannels.clear()
                   listAllChannels.addAll(response)
                   adapter.notifyItemRangeInserted(0, response.size)

                   val response2 = withContext(Dispatchers.IO) {
                       readMyMsg()
                   }
                   val tmp = listAllChannels.size
                   listAllChannels.addAll(response2)
                   adapter.notifyItemRangeInserted(tmp, listAllChannels.size - tmp)
                }
            }
            handler.postDelayed(runnable, 1.toLong())
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putStringArrayList(KEY_CHANNELS, listAllChannels)
        super.onSaveInstanceState(outState)
    }

    override fun onStop() {
        Log.d(KEY_LOG, "ON_STOP")
        super.onStop()
    }

    override fun onDestroy() {
        Log.d(KEY_LOG, "ON_DESTROY")
        super.onDestroy()
    }

    private fun readAllChats() : Array<String> {
        return try {
            MyApp.staticInstance?.serversWorkService?.getListChannels()?.execute()?.body()!!
        } catch (e : Exception) {
            e.printStackTrace()
            Toast.makeText(activity,
                "НЕТ ИНТЕРНЕТА, мы проиграли в чаты", Toast.LENGTH_SHORT).show()
            arrayOf()
        }
    }

    private fun readMyMsg() : ArrayList<String> {
        val ans = ArrayList<String>()
        var count = 0
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
                    if (!ans.contains(i.from)) {
                        ans.add(i.from)
                    }
                }
                if (i.from == "Moahim2") {
                    if (!i.to.contains("@") && !ans.contains(i.to)) {
                        ans.add(i.to)
                    }
                }
            }
        }
        return ans
    }

    companion object {
        private const val KEY_LOG = "ListChatFragments"
    }
}