/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :Levitskiy Ivan
 */
class Solution : MonotonicClock {
    private var a1 by RegularInt(0)
    private var a2 by RegularInt(0)
    private var b1 by RegularInt(0)
    private var b2 by RegularInt(0)
    private var r by RegularInt(0)

    override fun write(time: Time) {
        // write right-to-left
        a2 = time.d1
        b2 = time.d2
        r = time.d3
        b1 = time.d2
        a1 = time.d1
    }

    override fun read(): Time {
        // read left-to-right
        val v11 = a1
        val v12 = b1
        val w = r
        val v22 = b2
        val v21 = a2
        return if (v11 == v21) {
            if (v12 == v22) {
                Time(v11, v12, w)
            } else {
                Time(v11, v22, 0)
            }
        } else {
            Time(v21, 0, 0)
        }
    }
}