package hw.hw3

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.widget.Toast
import androidx.core.content.ContextCompat
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import android.Manifest
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import androidx.core.app.ActivityCompat

class MainActivity : AppCompatActivity() {
    private val idReadContactsPermission = 1

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_CONTACTS)
            != PackageManager.PERMISSION_GRANTED
        ) {
            ActivityCompat.requestPermissions(
                this,
                arrayOf(Manifest.permission.READ_CONTACTS),
                idReadContactsPermission
            )
        } else {
            createActivity()
        }
    }


    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        when(requestCode) {
            idReadContactsPermission -> {
                if (grantResults.isNotEmpty() && grantResults[0]
                    == PackageManager.PERMISSION_GRANTED) {
                    createActivity()
                } else {
                    Toast.makeText(
                        this@MainActivity, "Mistake: the application cannot generate" +
                            " a list of contacts without access to them", Toast.LENGTH_LONG).show()
                }
            }
        }
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
    }

    private fun createActivity() {
        val viewManager = LinearLayoutManager(this)
        val listContacts = findViewById<RecyclerView>(R.id.list_contacts)
        listContacts.setHasFixedSize(true)

        listContacts.apply {
            layoutManager = viewManager
            adapter =
                ContactAdapter(this@MainActivity.fetchAllContacts(), {
                    startActivity(Intent(
                        Intent.ACTION_DIAL, Uri.parse("tel: ${it.phoneNumber}")))
                }) {
                    intent = Intent(Intent.ACTION_VIEW, Uri.fromParts(
                        "sms", "tel: ${it.phoneNumber}", null))
                    intent.putExtra("sms_body", "Hello")
                    startActivity(intent)
                }
            Toast.makeText(
                this@MainActivity,
                this.adapter?.itemCount?.let {
                    resources.getQuantityString(R.plurals.numberOfContacts,
                        it, it)
                }, Toast.LENGTH_LONG).show()
        }
    }
}