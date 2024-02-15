package hw.hw4

import android.graphics.Bitmap
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import androidx.appcompat.widget.AppCompatTextView
import androidx.recyclerview.widget.RecyclerView

data class Message(val contactInformation: String, var text_message: String?,
                   val id : Int, var image : Bitmap?, val path : String?) {

}

class MessageViewHolder(root: View) : RecyclerView.ViewHolder(root) {
    private val contactInformation: TextView = root.findViewById(R.id.contact_information)
    private val textMessage: AppCompatTextView = root.findViewById(R.id.text_message)
    val imageView: ImageView = root.findViewById(R.id.imageInMessage)

    fun bind(message : Message) {
        contactInformation.text = message.contactInformation
        textMessage.text = message.text_message
        imageView.setImageBitmap(message.image)
    }

}

class MessageAdapter(
    private val messages : ArrayList<Message>,
    private val onClickImage : (Message) -> Unit,
): RecyclerView.Adapter<MessageViewHolder>() {

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): MessageViewHolder {
        val holder = MessageViewHolder(
            LayoutInflater.from(parent.context).inflate(R.layout.message, parent, false)
        )
        holder.imageView.setOnClickListener {
            onClickImage(messages[holder.adapterPosition])
        }
        return holder
    }

    override fun onBindViewHolder(
        holder: MessageViewHolder, position: Int
    ) = holder.bind(messages[position])

    override fun getItemCount() = messages.size


    fun updateElement(position: Int, bitmap: Bitmap) {
        messages[position - messages[0].id].image = bitmap
        messages[position - messages[0].id].text_message = null
        notifyItemChanged(position - messages[0].id)
    }

    fun add(message : Message) {
        messages.add(message)
        notifyItemInserted(messages.size)
    }

}
