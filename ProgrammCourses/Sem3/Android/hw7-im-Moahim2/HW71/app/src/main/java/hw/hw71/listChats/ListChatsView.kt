package hw.hw71.listChats

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import hw.hw71.R

class ChatViewHolder(parent: ViewGroup, private val onClickChat: (View, String) -> Unit) : RecyclerView.ViewHolder(
    LayoutInflater.from(parent.context).inflate(R.layout.chat, parent, false)
) {
    init {
        itemView.setOnClickListener {
            onClickChat(itemView, chatName.text.toString())
        }
    }

    private val chatName : TextView = itemView.findViewById(R.id.chat_name)

    fun bind(chatFragmentName : String) {
        chatName.text = chatFragmentName
    }

}


class ChatAdapter(
    private val chats: ArrayList<String>,
    private val onClickChat: (View, String) -> Unit,
) : RecyclerView.Adapter<ChatViewHolder>() {
    init {
        println("chats.size : ${chats.size}")
    }
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int) = ChatViewHolder(parent, onClickChat)

    override fun onBindViewHolder(holder: ChatViewHolder, position: Int) = holder.bind(chats[position])

    override fun getItemCount() = chats.size
}

