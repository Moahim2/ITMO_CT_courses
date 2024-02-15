package hw.hw71.chat

import android.graphics.Bitmap
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.TextView
import androidx.annotation.NonNull
import androidx.appcompat.widget.AppCompatTextView
import androidx.recyclerview.widget.RecyclerView
import androidx.room.*
import hw.hw71.R


@Entity
data class Message(
    @ColumnInfo val contactInformation: String,
    @ColumnInfo var text_message: String? = null,
    @PrimaryKey val id : Int,
    @ColumnInfo val channel : String,
    @Embedded var image : Bitmap?,
    @ColumnInfo val path : String,
    @ColumnInfo var absolutelyPath : String,
)

@Dao
interface MessageDao {
    @Query("SELECT * FROM message WHERE channel LIKE :channel")
    fun getAll(channel: String) : List<Message>

    @Query("SELECT * \n" +
            "    FROM message \n" +
            "    WHERE   ID =" +
            " (SELECT MAX(ID)  FROM message)")
    fun getMaxID() : Message

    @Query("SELECT * \n" +
            "    FROM message \n" +
            "    WHERE  ID = :id")
    fun getOnID(id : Int) : Message?

    @Update
    fun update(message: Message)

    @Insert
    fun insert(Message : Message)
}

@Database(entities = [Message::class], version = 1, exportSchema = true)
abstract class AppDatabase : RoomDatabase() {
    abstract fun messageDao(): MessageDao?
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
    private val messages: ArrayList<Message>,
    private val onClickImage: (Message) -> Unit,
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
    ) {
        holder.bind(messages[position])
    }

    override fun getItemCount() = messages.size


    fun updateElement(position: Int) {
        notifyItemChanged(position)
    }

    fun add(lastPos : Int) {
        notifyItemRangeInserted(lastPos + 1, messages.size - lastPos - 1)
    }

}
