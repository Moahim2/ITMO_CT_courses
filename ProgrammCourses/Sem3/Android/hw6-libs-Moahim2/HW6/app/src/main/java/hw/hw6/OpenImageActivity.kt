package hw.hw6

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.ImageView
import com.squareup.picasso.Picasso

class OpenImageActivity : AppCompatActivity() {
    lateinit var image : ImageView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_open_image)
        image = findViewById(R.id.bigImage)
        Picasso.get().load(
            "http://213.189.221.170:8008/img/" + intent?.getStringExtra("path")
        ).tag(OpenImageActivity::class.java)
         .placeholder(R.drawable.ic_launcher_foreground)
         .error(R.drawable.ic_launcher_background)
         .into(image)

    }

    override fun onDestroy() {
        super.onDestroy()
        Picasso.get().cancelTag(OpenImageActivity::class.java)
    }
}