package hw.hw4

import android.content.ComponentName
import android.content.Intent
import android.content.ServiceConnection
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.os.IBinder
import android.util.Log
import android.widget.ImageView

class OpenImageActivity : AppCompatActivity() {
    lateinit var image : ImageView


    var myIntentService : MyIntentService? = null
    var isIntentBound = false
    private val boundIntentServiceConnection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName, service: IBinder) {
            val binderBridge: MyIntentService.MyBinder = service as MyIntentService.MyBinder
            myIntentService = binderBridge.getMyService()
            Log.d("OPEN_IMAGE_ACTIVITY", "INTENT_SERVICE_CONNECT")
            isIntentBound = true
            if (myIntentService!!.curBigBitMap != null) {
                image.setImageBitmap(myIntentService!!.curBigBitMap)
            }
        }

        override fun onServiceDisconnected(name: ComponentName) {
            Log.d("OPEN_IMAGE_ACTIVITY", "INTENT_SERVICE_DISCONNECT")
            isIntentBound = false
            myIntentService = null
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_open_image)
        image = findViewById(R.id.bigImage)
    }

    override fun onStart() {
        super.onStart()
        val starterMyIntentService = Intent(this, MyIntentService::class.java)
        startService(starterMyIntentService)
        bindService(starterMyIntentService, boundIntentServiceConnection, BIND_AUTO_CREATE)
    }

    override fun onStop() {
        super.onStop()
        unbindService(boundIntentServiceConnection)
    }
}