package hw.hw6

import android.app.Service
import android.content.Intent
import android.os.Binder
import android.os.IBinder
import android.util.Log

class MyStoreService : Service() {
    private var binder = MyStoreBinder()

    var currentListMessage : ArrayList<Message> = ArrayList()

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
