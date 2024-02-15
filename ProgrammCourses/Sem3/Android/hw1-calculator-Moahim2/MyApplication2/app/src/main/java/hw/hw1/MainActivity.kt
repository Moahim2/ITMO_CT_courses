package hw.hw1

import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Button
import android.widget.TextView


const val BUNDLE_KEY = "MY_KEY"

val tableMyButtonsID = arrayOf(
    R.id.b0,
    R.id.b1,
    R.id.b2,
    R.id.b3,
    R.id.b4,
    R.id.b5,
    R.id.b6,
    R.id.b7,
    R.id.b8,
    R.id.b9,
    R.id.add,
    R.id.sub,
    R.id.mul,
    R.id.div,
    R.id.clear,
    R.id.eqv,
    R.id.dot,
    R.id.copy,
)

val tableMyButtonsChar = arrayOf(
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', '*', '/', 'C', '=', '.', 'c',
)

var calcSaveInformation : String = ""
var isEqv : Boolean = false

class MyButton (
    private val char : Char, clicker : TextView, output : TextView, failedOutput : TextView,
    calc : Calculator, myClipBoardManager : ClipboardManager) {
    init {
        clicker.setOnClickListener {
            if (!calc.parse(char)) {
                failedOutput.text = calc.takeExceptionMessage()
            } else {
                failedOutput.text = ""
            }
            calcSaveInformation = calc.calculatorOutput
            isEqv = calc.takeFlagEqv()
            output.text = calcSaveInformation
            if (char == 'c') {
                myClipBoardManager.setPrimaryClip(ClipData.newPlainText("", calcSaveInformation))
            }
        }
    }
}

lateinit var output : TextView
lateinit var failedOutput : TextView

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val calculator = Calculator(calcSaveInformation, isEqv)
        val myButtons: ArrayList<MyButton> = ArrayList()
        val myClipBoard = getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
        output = findViewById(R.id.result)
        failedOutput = findViewById(R.id.my_exception)

        for (i in tableMyButtonsID.indices) {
            myButtons.add(
                MyButton(
                    tableMyButtonsChar[i], findViewById<Button>(tableMyButtonsID[i]),
                    output, failedOutput, calculator, myClipBoard
                )
            )
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putString(BUNDLE_KEY, calcSaveInformation)
        outState.putBoolean(BUNDLE_KEY, isEqv)
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        calcSaveInformation = savedInstanceState.getString(BUNDLE_KEY, calcSaveInformation)
        isEqv = savedInstanceState.getBoolean(BUNDLE_KEY, isEqv)
        output.text = calcSaveInformation
    }

    override fun onPause() {
        super.onPause()
        if (isFinishing) {
            calcSaveInformation = ""
            isEqv = false
        }
    }
}