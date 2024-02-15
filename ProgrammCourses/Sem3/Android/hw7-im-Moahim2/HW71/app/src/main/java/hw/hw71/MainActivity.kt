package hw.hw71

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.view.View
import androidx.fragment.app.FragmentManager
import hw.hw71.chat.ChatFragment
import hw.hw71.listChats.ListChatFragments
import hw.hw71.listChats.OnChatClick

class MainActivity : AppCompatActivity(), OnChatClick {

    private var fragmentExtraContainer: View? = null //right
    private var fragmentContainer: View? = null //left

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        Log.d(KEY_LOG, "ON_CREATE")

        fragmentExtraContainer = findViewById(R.id.fragment_extra_container)
        fragmentContainer = findViewById(R.id.fragment_container)

        if (savedInstanceState == null) {
            Log.d(KEY_LOG, "ON_CREATE_FIRST_LAUNCH")
            supportFragmentManager
                .beginTransaction()
                .add(R.id.fragment_container, ListChatFragments(), TAG_LIST_CHAT_FRAGMENTS)
                .commit()

        } else if (fragmentExtraContainer != null) {
            Log.d(KEY_LOG, "ON_CREATE__Portrait->Landscape__")
            val chatFragment = supportFragmentManager.findFragmentByTag(TAG_CHAT_FRAGMENT)
            if (chatFragment != null) {
                println("input : ${supportFragmentManager.fragments}")
                supportFragmentManager.popBackStack(
                    TAG_CHAT_FRAGMENT,
                    FragmentManager.POP_BACK_STACK_INCLUSIVE
                )
                supportFragmentManager.executePendingTransactions()

                supportFragmentManager
                    .beginTransaction()
                    .replace(R.id.fragment_extra_container, chatFragment, TAG_CHAT_FRAGMENT)
                    .commit()

                println("output : ${supportFragmentManager.fragments}")
            }
        } else if (fragmentContainer != null) {
            Log.d(KEY_LOG, "ON_CREATE__Landscape->Portrait__")

            val chatFragment = supportFragmentManager.findFragmentByTag(TAG_CHAT_FRAGMENT)
            if (chatFragment != null) {
                println("input : ${supportFragmentManager.fragments}")
                //supportFragmentManager.beginTransaction().remove(listFragment!!).commit()
                supportFragmentManager.beginTransaction().remove(chatFragment).commit()
                supportFragmentManager.popBackStack(
                    TAG_CHAT_FRAGMENT,
                    0
                )
                supportFragmentManager.executePendingTransactions()

                supportFragmentManager
                    .beginTransaction()
                    .replace(R.id.fragment_container, chatFragment, TAG_CHAT_FRAGMENT)
                    .addToBackStack(TAG_CHAT_FRAGMENT) //+кнопка назад
                    .commit()
                println("output : ${supportFragmentManager.fragments}")
            }
        }
    }


    override fun onClickChat(root: View, nameChannel : String) {
        Log.d(KEY_LOG, "ON_Click_Chat")

        val chatFragment = ChatFragment.create(nameChannel)
        if (fragmentExtraContainer != null) {
            println("ClickInLandscape")

            supportFragmentManager
                .beginTransaction()
                .replace(
                    R.id.fragment_extra_container,
                    chatFragment,
                    TAG_CHAT_FRAGMENT
                )
                .commit()
        } else {
            println("ClickInPortrait")
            println("input : ${supportFragmentManager.fragments}")
            supportFragmentManager
                .beginTransaction()
                .replace(
                    R.id.fragment_container,
                    chatFragment,
                    TAG_CHAT_FRAGMENT
                )
                .addToBackStack(TAG_CHAT_FRAGMENT)
                .commit()
            println("output : ${supportFragmentManager.fragments}")
        }
    }

    override fun onPause() {
        Log.d(KEY_LOG, "ON_PAUSE")
        super.onPause()
    }

    override fun onDestroy() {
        Log.d(KEY_LOG, "ON_DESTROY")
        super.onDestroy()
    }

    companion object {
        private const val TAG_LIST_CHAT_FRAGMENTS = "list"
        private const val TAG_CHAT_FRAGMENT = "chat_fragment"
        private const val KEY_LOG: String = "MAIN_ACTIVITY"
    }

}

