package hw.hw4

import android.graphics.BitmapFactory
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.ImageView

class OpenImageActivity : AppCompatActivity() {
    lateinit var image : ImageView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_open_image)
        image = findViewById(R.id.bigImage)
        val byteArray = intent.extras?.getByteArray("imageBytesArray")
        val bitmap = BitmapFactory.decodeByteArray(byteArray, 0, byteArray!!.size)
        image.setImageBitmap(bitmap)
    }



}