package com.example.hw2

import android.content.res.Configuration
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.inputmethod.EditorInfo
import android.widget.Button
import android.widget.LinearLayout
import android.widget.TextView
import com.google.android.material.textfield.TextInputEditText
import java.util.*

class MainActivity : AppCompatActivity() {
    private lateinit var mainLinearLayout : LinearLayout
    private lateinit var login_button : Button
    private lateinit var email : TextInputEditText
    private lateinit var password : TextInputEditText
    private lateinit var messenger : TextView
    private val BUNDLE_KEY = "MY_KEY"
    private var saveInformation = R.string.noInfo
    private var isEmail : Regex = Regex("[a-zA-Z0-9._-]+@[a-z]+\\.+[a-z]+")


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        mainLinearLayout = findViewById(R.id.mainLLayout)
        login_button = findViewById(R.id.LOGIN)
        email = findViewById(R.id.email)
        password = findViewById(R.id.password)
        messenger = findViewById(R.id.message)
        login_button.setOnClickListener {
            saveInformation = if (email.text?.isEmpty() == true) {
                R.string.noLogin
            } else if (email.text?.matches(isEmail) != true) {
                R.string.incorrectEmail
            } else if (password.text?.isEmpty() == true) {
                R.string.noPassword
            } else {
                R.string.incorrectlyLoginOrPassword
            }
            messenger.setText(saveInformation)
        }

        password.setOnEditorActionListener { _, k, _ ->
            if (k == EditorInfo.IME_ACTION_NEXT) {
                login_button.callOnClick()
                return@setOnEditorActionListener true
            } else {
                return@setOnEditorActionListener false
            }
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putInt(BUNDLE_KEY, saveInformation)
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        saveInformation = savedInstanceState.getInt(BUNDLE_KEY, saveInformation)
        messenger.setText(saveInformation)
        changeActivityParameters()
    }

    override fun onStart() {
        super.onStart()
        changeActivityParameters()
    }


    private fun changeActivityParameters() {
        if (resources.configuration.orientation == Configuration.ORIENTATION_PORTRAIT) {
            mainLinearLayout.orientation = LinearLayout.VERTICAL
            mainLinearLayout.layoutParams.height = LinearLayout.LayoutParams.MATCH_PARENT
        } else {
            mainLinearLayout.orientation = LinearLayout.HORIZONTAL
            mainLinearLayout.layoutParams.height = LinearLayout.LayoutParams.WRAP_CONTENT
        }

        if (Locale.getDefault().language.equals("iw")) {
            mainLinearLayout.layoutDirection = LinearLayout.LAYOUT_DIRECTION_RTL
        } else {
            mainLinearLayout.layoutDirection = LinearLayout.LAYOUT_DIRECTION_LTR
        }
    }

    override fun onPause() {
        super.onPause()
        if (isFinishing) {
            saveInformation = R.string.noInfo
        }
    }
}
