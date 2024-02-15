package hw.hw3

import android.content.Context
import android.provider.ContactsContract
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView

data class Contact(val name: String, val phoneNumber: String)

fun Context.fetchAllContacts(): List<Contact> {
    contentResolver.query(
        ContactsContract.CommonDataKinds.Phone.CONTENT_URI,
        null, null, null, null
    ).use { cursor ->
            if (cursor == null) return emptyList()
            val builder = ArrayList<Contact>()
            while (cursor.moveToNext()) {
                val numberOfName = cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME)
                val name = if (numberOfName > -1) cursor.getString(numberOfName) else "N/A"

                val numberOfPhoneNumber = cursor.getColumnIndex(ContactsContract.CommonDataKinds.Phone.NUMBER)
                val phoneNumber =
                    if (numberOfPhoneNumber > -1) cursor.getString(numberOfPhoneNumber) else "N/A"

                builder.add(Contact(name, phoneNumber))
            }
            return builder
        }
}

////////

class ContactViewHolder(root: View) : RecyclerView.ViewHolder(root) {
    val name: TextView = root.findViewById(R.id.name)
    val phoneNumber: TextView = root.findViewById(R.id.phone_number)
    val createSMS: Button = root.findViewById(R.id.createSMS)

    fun bind(contact : Contact) {
        name.text = contact.name
        phoneNumber.setText(R.string.number_field)
        phoneNumber.append(contact.phoneNumber)
    }
}

class ContactAdapter(
    private val contacts : List<Contact>,
    private val onClickCall : (Contact) -> Unit,
    private val onClickSendSMS : (Contact) -> Unit,
): RecyclerView.Adapter<ContactViewHolder>() {

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ContactViewHolder {
        val holder = ContactViewHolder(
            LayoutInflater.from(parent.context).inflate(R.layout.contact, parent, false)
        )

        holder.name.setOnClickListener {
            onClickCall(contacts[holder.absoluteAdapterPosition])
        }

        holder.phoneNumber.setOnClickListener {
            onClickCall(contacts[holder.absoluteAdapterPosition])
        }

        holder.createSMS.setOnClickListener {
            onClickSendSMS(contacts[holder.absoluteAdapterPosition])
        }
        return holder
    }

    override fun onBindViewHolder(
        holder: ContactViewHolder, position: Int
    ) = holder.bind(contacts[position])

    override fun getItemCount() = contacts.size
}
