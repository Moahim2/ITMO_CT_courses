package hw.hw6

import com.squareup.moshi.Json


data class ToServerJSONMessage(
    @Json(name = "from") val from: String = "none",
    @Json(name = "to") val to: String = "1@channel",
    @Json(name = "data") val `data`: Map<String, Map<String, String>>,
)


