package hw.hw8base

import android.os.*
import android.widget.ImageView
import androidx.appcompat.app.AppCompatActivity
import androidx.interpolator.view.animation.FastOutSlowInInterpolator


class MainActivity : AppCompatActivity() {
    private lateinit var myCustomView : MyCustomView

    private lateinit var firstView: ImageView
    private lateinit var secondView: ImageView
    private lateinit var mediumView: ImageView

    private var typeFirst = true
    private var typeSecond = true
    private var typeSinh = 0
    private var typeMedium = 0

    private fun firstAnimate() {
        val d = if (typeFirst) {
            typeFirst = false
            -1.5f
        } else {
            typeFirst = true
            1.5f
        }
        firstView.animate()
            .scaleX(d)
            .setInterpolator(FastOutSlowInInterpolator())
            .setDuration(2500L)
            .withEndAction(this::firstAnimate)
    }

    private fun secondAnimate() {

        val d = if (typeSecond) {
            typeSecond = false
//            t = -140f
            -1.5f
        } else {
            typeSecond = true
//            t = 70f
            1.5f
        }
        val t = when (typeSinh) {
            0 -> {
                -135f
            }
            1 -> {
                70f
            }
            else -> {
                0f
            }
        }
        typeSinh = (typeSinh + 1) % 4
        secondView.animate()
            .scaleX(d)
            .setInterpolator(FastOutSlowInInterpolator())
            .setDuration(1000L)
            .translationY(t)
            .withEndAction(this::secondAnimate)
    }

    private fun mediumAnimate() {
        when (typeMedium) {
            0 -> {
                typeMedium = 1
                mediumView.animate()
                    .scaleYBy(-1f)
                    .setInterpolator(FastOutSlowInInterpolator())
                    .setDuration(100L)
                    .withEndAction(this::mediumAnimate)

            }
            1 -> {
                typeMedium = 2
                mediumView.animate()
                    .scaleY(0f)
                    .setDuration(1950L).withEndAction(this::mediumAnimate)
            }
            else -> {
                typeMedium = 0
                mediumView.animate()
                    .scaleYBy(1f)
                    .setInterpolator(FastOutSlowInInterpolator())
                    .setDuration(750L).withEndAction(this::mediumAnimate)

            }
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        myCustomView = findViewById(R.id.myView)
        firstView = findViewById(R.id.first)
        secondView = findViewById(R.id.second)
        mediumView = findViewById(R.id.medium)
        firstAnimate()
        secondAnimate()
        mediumAnimate()
    }
}

