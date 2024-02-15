package hw.hw4

import android.app.Service
import android.content.Intent
import android.os.Binder
import android.os.IBinder
import android.util.Log
import java.util.concurrent.CopyOnWriteArrayList

class MyStoreService : Service() {
    private var binder = MyStoreBinder()

    var currentStoreListMessage : ArrayList<Message> = ArrayList()

    fun saveAllList(listMessages : CopyOnWriteArrayList<Message>) {
        currentStoreListMessage = copyAllList(listMessages)
    }

    private fun copyAllList(listMessages : CopyOnWriteArrayList<Message>) : ArrayList<Message> {
        val copy = ArrayList<Message>()
        for (message in listMessages) {
            val messageCopy = message.copy()
            if (message.image != null) {
                messageCopy.image = message.image!!.copy(message.image!!.config, true)
            }
            copy.add(messageCopy)
        }
        return copy
    }

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        super.onStartCommand(intent, flags, startId)
        return START_NOT_STICKY
    }

    override fun onBind(intent: Intent?): IBinder {
        return binder
    }

    override fun onDestroy() {
        super.onDestroy()
        Log.d("STORE_SERVICE", "ON_DESTROY")
    }

    inner class MyStoreBinder : Binder() {
        fun getMyService() = this@MyStoreService
    }
}