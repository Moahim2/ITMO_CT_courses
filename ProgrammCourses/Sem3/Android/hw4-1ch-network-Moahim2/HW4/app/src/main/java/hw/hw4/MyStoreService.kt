package hw.hw4

import android.app.Service
import android.content.Intent
import android.os.Binder
import android.os.IBinder
import android.util.Log

class MyStoreService : Service() {
    private var binder = MyStoreBinder()

    var currentStoreListMessage : ArrayList<Message> = ArrayList()

    fun saveAllList(listMessages : ArrayList<Message>) {
        currentStoreListMessage = copyAllList(listMessages)
    }

    fun copyAllList(listMessages : ArrayList<Message>) : ArrayList<Message> {
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
        Log.d("StoreService", "LOOOOOOOOOOOOOOOOOSSSSSSSSEEEEEEEEEEEEEEEE")
    }

    inner class MyStoreBinder : Binder() {
        fun getMyService() = this@MyStoreService
    }
}