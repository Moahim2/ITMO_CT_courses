package hw.hw71.listChats

import android.view.View

interface OnChatClick {
    fun onClickChat(root : View, nameChannel : String)
}