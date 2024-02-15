package hw.hw6

import android.app.Application
import androidx.room.Room
import com.squareup.moshi.*
import com.squareup.moshi.kotlin.reflect.KotlinJsonAdapterFactory
import okhttp3.MultipartBody
import okhttp3.OkHttpClient
import okhttp3.RequestBody
import okhttp3.ResponseBody
import retrofit2.Call
import retrofit2.Retrofit
import retrofit2.converter.moshi.MoshiConverterFactory
import retrofit2.http.*
import java.util.concurrent.TimeUnit


interface ServersWorkService {

    @GET("1ch?")
    fun getListNewMessage(
        @Query("lastKnownId") lastId : Int,
        @Query("limit") limit : Int
    ) : Call<List<OnServerJSONMessage>>

    @Headers("Content-Type: application/json")
    @POST("1ch")
    fun sendTextMessage(@Body toServerJSONMessage : ToServerJSONMessage) : Call<ResponseBody>

//    @Headers("Content-Type: multipart/form-data; boundary=" +
//        "------" + 1667063270736 + "------")
    @Multipart
    @POST("1ch")
    fun sendImageMessage(
        @Part("msg") msg : RequestBody,
        @Part picture : MultipartBody.Part
    ) : Call<ResponseBody>

    @Streaming
    @GET("thumb/{pathToPicture}")
    fun loadMiniImage(@Path("pathToPicture") path : String) : Call<ResponseBody>

//    @Streaming
//    @GET("img/{pathToPicture}")
//    fun loadBigImage(@Path("pathToPicture") path : String) : Call<ResponseBody>
}

class MyApp : Application() {//обертка
private var myRetrofit: Retrofit? = null
var serversWorkService : ServersWorkService? = null
var myDatabase : AppDatabase? = null
var moshiWriterSendingMessage : JsonAdapter<ToServerJSONMessage>? = null

    companion object {
        var staticInstance: MyApp? = null
    }

    override fun onCreate() {
        println("createMyApp")
        staticInstance = this@MyApp

        myDatabase = Room.databaseBuilder(
            this, AppDatabase::class.java, "database-HW45").build()


        myRetrofit = Retrofit.Builder()
            .baseUrl("http://213.189.221.170:8008/")
            .addConverterFactory(MoshiConverterFactory.create(Moshi.Builder()
                .addLast(KotlinJsonAdapterFactory())
                .build()))
            .client(OkHttpClient.Builder()
                .readTimeout(5, TimeUnit.MINUTES)
                .connectTimeout(5, TimeUnit.MINUTES)
                .build())
            .build()

        serversWorkService = myRetrofit!!.create(ServersWorkService::class.java)
        moshiWriterSendingMessage = Moshi.Builder()
            .add(KotlinJsonAdapterFactory())
            .build().adapter(ToServerJSONMessage::class.java)



        super.onCreate()
    }

}





fun serverMessageToRecyclerViewMessage(onServerJSONMessage: OnServerJSONMessage) : Message {
    val textMes = if (onServerJSONMessage.data.containsKey("Image")) {
        "...DownloadImage..."
    } else {
        onServerJSONMessage.data["Text"]?.get("text")
    }
    val path = if (!onServerJSONMessage.data.containsKey("Image")) {
        ""
    } else {
        onServerJSONMessage.data["Image"]?.get("link")
    }

    return Message(
        "Name : ${onServerJSONMessage.from}, UNIX_Time : ${onServerJSONMessage.time}",
        textMes,
        onServerJSONMessage.id,
        null,
        path!!,
        ""
    )
}









