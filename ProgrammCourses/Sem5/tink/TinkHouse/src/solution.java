import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.function.BiFunction;

public class solution {
    private HttpURLConnection connection;

    private final String url;
    private final long srcName;

    private final MyLong counterSerial = new MyLong(1);
    private static long startTime = 0L;
    private static long curTime = 0L;
    private static boolean flagSync = true;


    public static final Charset CHARSET = StandardCharsets.UTF_8;
    public static final int ERROR_EXIT_CODE = 99;
    public static final int DEFAULT_EXIT_CODE = 0;

    private static final long BROADCAST_DST = 16383L;
    private static final String MY_NAME = "SmartHub1";



    private enum DevType {
        SMART_HUB (1),
        ENV_SENSOR (2),
        SWITCH (3),
        LAMP (4),
        SOCKET (5),
        CLOCK (6);

        private final byte code;

        DevType(int code) {
            this.code = (byte) code;
        }

        public byte getCode() {
            return this.code;
        }

        public static DevType getVal(byte b) {
            return switch (b) {
               case 1 -> SMART_HUB;
               case 2 -> ENV_SENSOR;
               case 3 -> SWITCH;
               case 4 -> LAMP;
               case 5 -> SOCKET;
               case 6 -> CLOCK;
               default -> throw new IllegalArgumentException("Unexpected value: " + b);
            };
        }
    }

    private enum CMD {
        WHO_IS_HERE (1),
        I_AM_HERE (2),
        GET_STATUS (3),
        STATUS (4),
        SET_STATUS (5),
        TICK (6);

        private final byte code;

        CMD(int code) {
            this.code = (byte) code;
        }

        public byte getCode() {
            return this.code;
        }

        public static CMD getVal(byte b) {
            return switch (b) {
                case 1 -> WHO_IS_HERE;
                case 2 -> I_AM_HERE;
                case 3 -> GET_STATUS;
                case 4 -> STATUS;
                case 5 -> SET_STATUS;
                case 6 -> TICK;
                default -> throw new IllegalArgumentException("Unexpected value CMD: " + b);
            };
        }

    }


    private final static class MyLong {
        long cur;
        public MyLong(long cur) {
            this.cur = cur;
        }

        public long inc() {
            return cur++;
        }
    }

    private static final class Base64Helper {
        private static final Base64.Encoder base64encoder = Base64.getUrlEncoder().withoutPadding();
        private static final Base64.Decoder base64decoder = Base64.getUrlDecoder();


        private static String encodeBase64(byte[] date) {
            return base64encoder.encodeToString(date);
        }


        private static byte[] decodeBase64(String date) {
            return base64decoder.decode(date);
        }
    }






    private static final class Coder {

        private static class MyInt {
            int cur;
            public MyInt(int cur) {
                this.cur = cur;
            }

            public int inc() {
                return cur++;
            }
        }

        private static byte[] encodeString(String string) {
            byte[] bytes = new byte[string.length() + 1];
            bytes[0] = (byte) string.length();
            byte[] strBytes = string.getBytes(CHARSET);
            System.arraycopy(strBytes, 0, bytes, 1, string.length());
            return bytes;
        }

        private static String decodeString(byte[] bytes, MyInt myInt) {
            int length = (bytes[myInt.inc()] + 256) % 256;
            byte[] strBytes = new byte[length];
            for (int i = 0; i < length; i++) {
                strBytes[i] = bytes[myInt.inc()];
            }
            return new String(strBytes, 0, length, CHARSET);
        }

        private static List<Byte> encodeVARUINT(long value) {
            ArrayList<Byte> listBytes = new ArrayList<>();

            do {
                byte b = (byte)(value & 0x7F);
                value >>= 7;
                if (value != 0)
                    b |= 0x80;
                listBytes.add(b);
            } while (value != 0);

            return listBytes;
        }

        private static Long decodeVARUINT(byte[] bytes, MyInt myInt) {
            long result = 0;
            long shift = 0;
            while (true) {
                long b = bytes[myInt.inc()];
                result |= (b & 0x7F) << shift;
                if ((b & 0x80) == 0) {
                    break;
                }
                shift += 7;
            }
            return result;
        }


        private static byte calcCrc8(List<Byte> data, int endIndex) {
            byte generator = 0x1D;
            byte crc = 0;

            for (int j = 0; j <= endIndex; j++)
            {
                byte currByte = data.get(j);
                crc ^= currByte; /* XOR-in the next input byte */

                for (int i = 0; i < 8; i++)
                {
                    if ((crc & 0x80) != 0)
                    {
                        crc = (byte)((crc << 1) ^ generator);
                    }
                    else
                    {
                        crc <<= 1;
                    }
                }
            }
            return crc;
        }

        private static byte calcCrc8(byte[] data, int startIndex, int endIndex) {
            byte generator = 0x1D;
            byte crc = 0;

            for (int j = startIndex; j <= endIndex; j++)
            {
                byte currByte = data[j];
                crc ^= currByte; /* XOR-in the next input byte */

                for (int i = 0; i < 8; i++)
                {
                    if ((crc & 0x80) != 0)
                    {
                        crc = (byte)((crc << 1) ^ generator);
                    }
                    else
                    {
                        crc <<= 1;
                    }
                }
            }
            return crc;
        }

        private static List<Byte> encodeTrigger(Trigger trigger) {
            ArrayList<Byte> list = new ArrayList<>();
            list.add(trigger.op);
            list.addAll(encodeVARUINT(trigger.value));
            list.addAll(byteArrayToList(encodeString(trigger.name)));
            return list;
        }

        private static EnvSensor decodeEnvSensor(byte[] bytes, MyInt myInt) {
            String deviceName = decodeString(bytes, myInt);
            byte sensors = bytes[myInt.inc()];
            List<Trigger> list = decodeArrT(bytes, myInt, Coder::decodeTrigger);
            Trigger[] triggers = new Trigger[list.size()];
            for (int i = 0; i < list.size(); i++) {
                triggers[i] = list.get(i);
            }
            return new EnvSensor(deviceName, sensors, triggers);
        }

        private static Trigger decodeTrigger(byte[] bytes, MyInt myInt) {
            byte op = bytes[myInt.inc()];
            long value = decodeVARUINT(bytes, myInt);
            return new Trigger(op, value, decodeString(bytes, myInt));
        }

        private static Strings decodeStrings(byte[] bytes, MyInt myInt) {
            String deviceName = decodeString(bytes, myInt);
            List<String> list = decodeArrT(bytes, myInt, Coder::decodeString);
            String[] strings = new String[list.size()];
            for (int i = 0; i < list.size(); i++) {
                strings[i] = list.get(i);
            }
            return new Strings(deviceName, strings);
        }

        private static Varuints decodeVaruints(byte[] bytes, MyInt myInt) {
            List<Long> list = decodeArrT(bytes, myInt, Coder::decodeVARUINT);
            long[] varuints = new long[list.size()];
            for (int i = 0; i < list.size(); i++) {
                varuints[i] = list.get(i);
            }
            return new Varuints(varuints);
        }

        private static <T> List<T> decodeArrT(byte[] bytes, MyInt myInt, BiFunction<byte[], MyInt, T> decodeT) {
            List<T> list = new ArrayList<>();
            for (int i = bytes[myInt.inc()]; i > 0; i--) {
                list.add(decodeT.apply(bytes, myInt));
            }
            return list;
        }

        private static Payload<?> decodePayload(byte[] bytes, MyInt myInt) {

            int length = bytes[myInt.inc()];
            int firstIndex = myInt.cur;


            long src = Coder.decodeVARUINT(bytes, myInt);
            long dst = Coder.decodeVARUINT(bytes, myInt);
            long serial = Coder.decodeVARUINT(bytes, myInt);
            byte d = bytes[myInt.inc()];
            byte c = bytes[myInt.inc()];
            DevType devType = DevType.getVal(d);
            CMD cmd = CMD.getVal(c);

            Payload.PayloadCreator creator = new Payload.PayloadCreator(src, dst, serial, d, c);

            Payload<?> payload = switch (cmd) {
                case WHO_IS_HERE, I_AM_HERE -> switch (devType) {
                    case ENV_SENSOR -> new Payload<>(creator, decodeEnvSensor(bytes, myInt));
                    case SWITCH -> new Payload<>(creator, decodeStrings(bytes, myInt));
                    case LAMP, SOCKET, CLOCK, SMART_HUB -> new Payload<>(creator, new Name(decodeString(bytes, myInt)));
                };
                case STATUS -> switch (devType) {
                    case ENV_SENSOR -> new Payload<>(creator, decodeVaruints(bytes, myInt));
                    case SWITCH, LAMP, SOCKET-> new Payload<>(creator, new OneByteDevice(bytes[myInt.inc()]));
                    default -> throw new IllegalArgumentException("Такие пакеты не должны приходить");
                };
                case TICK -> switch (devType) {
                    case CLOCK -> new Payload<>(creator, new Timer(decodeVARUINT(bytes, myInt)));
                    case SMART_HUB -> new Payload<>(creator, new Name(decodeString(bytes, myInt)));
                    default -> throw new IllegalArgumentException("Такие пакеты не должны приходить");
                };
                default -> throw new IllegalArgumentException("Такие пакеты не должны приходить");
            };


            if (calcCrc8(bytes, firstIndex, firstIndex + length - 1) != bytes[myInt.inc()]) {
                return null;
            }

            return payload;
        }

        private static List<Payload<?>> decodeAllRequests(String codingDate) {
            ArrayList<Payload<?>> list = new ArrayList<>();
            MyInt myInt = new MyInt(0);
            try {
                byte[] bytes = Base64Helper.decodeBase64(codingDate);
                while (myInt.cur < bytes.length) {
                    Payload<?> p = decodePayload(bytes, myInt);

                    //null возвращается только при ошибке контрольной суммы
                    if (p != null) {
                        list.add(p);
                    }

//                    Payload<?> ttt = list.get(list.size() - 1);
//                    if (ttt.cmd != 6) {
//                        System.out.println(list.get(list.size() - 1));
//                        System.out.println("--- CurTime: " + SmartHome.curTime);
//                    }
                }
//                System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");


                return list;
            } catch (IllegalArgumentException e) {
                return null;
            }
        }
    }




    private static class LAN_Structure {
        private static final Map<String, Payload<?>> mapNameToD = new HashMap<>();
        private static final Map<Long, Payload<?>> mapAdrToD = new HashMap<>();

        private static final Set<Long> offDevices = new HashSet<>();

        private static final Map<Long, Map.Entry<Long, Payload<?>>> setRequests = new HashMap<>();


        private static void addNewDevice(Payload<?> deviceMsg) {
            String name = deviceMsg.cmdBody.deviceName;
            long src = deviceMsg.src;
            mapNameToD.put(name, deviceMsg);
            mapAdrToD.put(src, deviceMsg);
            offDevices.remove(deviceMsg.src);
        }

        private static boolean workingDev(String name) {
            return mapNameToD.containsKey(name) && !offDevices.contains(mapNameToD.get(name).src);
        }

        private static boolean workingDev(Long addr) {
            return mapAdrToD.containsKey(addr) && !offDevices.contains(addr);
        }
    }




    private abstract static class Device {
        String deviceName = null;

        abstract List<Byte> encode();
    }


    private static class Timer extends Device {
        Long time;
        public Timer(Long varuint) {
            this.time = varuint;
        }

        @Override
        List<Byte> encode() {
            List<Byte> list = new ArrayList<>();
            if (deviceName != null) {
                list.addAll(byteArrayToList(Coder.encodeString(deviceName)));
            }
            list.addAll(Coder.encodeVARUINT(time));
            return list;
        }

        @Override
        public String toString() {
            return "timestamp: " + time;
        }
    }


    private static class Name extends Device {

        public Name(String name) {
            deviceName = name;
        }

        @Override
        List<Byte> encode() {
            return byteArrayToList(Coder.encodeString(deviceName));
        }

        @Override
        public String toString() {
            return "deviceName: " + deviceName;
        }
    }

    private static class Empty extends Device {

        @Override
        List<Byte> encode() {
            return Collections.emptyList();
        }

        @Override
        public String toString() {
            return "";
        }
    }

    private static class OneByteDevice extends Device {
        byte bfl;
        public OneByteDevice(byte bfl) {
            this.bfl = bfl;
        }

        @Override
        List<Byte> encode() {
            return List.of(bfl);
        }

        @Override
        public String toString() {
            return "byte " + bfl;
        }
    }

    private static class Trigger implements Comparable<Trigger> {
        byte op;
        long value;
        String name;

        int flag;
        int type;
        int index;

        public Trigger(byte op, long value, String name) {
            this.op = op;
            this.value = value;
            this.name = name;
            int[] decodeBytes = get4BitMask(op);
            flag = decodeBytes[3];
            type = decodeBytes[2];
            index = decodeBytes[1] + decodeBytes[0] * 2;
        }

        @Override
        public String toString() {
            return "Trigger{" +
                    "op=" + op +
                    ", value=" + value +
                    ", name='" + name + '\'' +
                    '}';
        }

        @Override
        public int compareTo(Trigger o) {
            Integer i = index;
            Integer j = o.index;
            return i.compareTo(j);
        }
    }

    private static class EnvSensor extends Device {
        byte sensors;
        Trigger[] triggers;

        public EnvSensor(String deviceName, byte sensors, Trigger... triggers) {
            this.deviceName = deviceName;
            this.sensors = sensors;
            this.triggers = triggers;
        }


        @Override
        List<Byte> encode() {
            ArrayList<Byte> list = new ArrayList<>(byteArrayToList(Coder.encodeString(deviceName)));
            list.add(sensors);
            for (Trigger t : triggers) {
                list.addAll(Coder.encodeTrigger(t));
            }
            return list;
        }

        @Override
        public String toString() {
            StringBuilder s = new StringBuilder();
            s.append("deviceName: ").append(deviceName).append("\n");
            s.append("sensors: ").append(sensors).append("\n");
            for (Trigger t : triggers) {
                s.append(t);
            }
            return s.toString();
        }
    }

    private static class Strings extends Device {
        String[] devNames;

        public Strings(String deviceName, String[] devNames) {
            this.deviceName = deviceName;
            this.devNames = devNames;
        }

        @Override
        List<Byte> encode() {
            List<Byte> list = new ArrayList<>();
            if (deviceName != null) {
                list.addAll(byteArrayToList(Coder.encodeString(deviceName)));
            }
            for (String s : devNames) {
                list.addAll(byteArrayToList(Coder.encodeString(s)));
            }
            return list;
        }

        @Override
        public String toString() {
            return "Strings{" +
                    ", deviceName='" + deviceName + '\'' + "\n" +
                    "devNames=" + Arrays.toString(devNames) + "\n" +
                    '}';
        }
    }


    private static class Varuints extends Device {
        long[] values;

        public Varuints(long[] values) {
            this.values = values;
        }

        @Override
        List<Byte> encode() {
            List<Byte> list = new ArrayList<>();
            for (long v : values) {
                list.addAll(Coder.encodeVARUINT(v));
            }
            return list;
        }

        @Override
        public String toString() {
            return "Varuints{" +
                    "values=" + Arrays.toString(values) + "\n" +
                    ", deviceName='" + deviceName + '\'' + "\n" +
                    '}';
        }
    }

    private static class Payload<T extends Device> {
        long src;
        long dst;
        long serial;
        byte devType;
        byte cmd;
        T cmdBody;

        private static final class PayloadCreator {
            Long src;
            Long dst;
            Long serial;
            byte devType;
            byte cmd;

            public PayloadCreator(Long src, Long dst, Long serial, byte devType, byte cmd) {
                this.src = src;
                this.dst = dst;
                this.serial = serial;
                this.devType = devType;
                this.cmd = cmd;
            }

        }

        public Payload(PayloadCreator creator, T cmdBody) {
            this.src = creator.src;
            this.dst = creator.dst;
            this.serial = creator.serial;
            this.devType = creator.devType;
            this.cmd = creator.cmd;
            this.cmdBody = cmdBody;
        }

        public Payload(long src, long dst, long serial, DevType devType, CMD cmd, T cmdBody) {
            this.src = src;
            this.dst = dst;
            this.serial = serial;
            this.devType = devType.getCode();
            this.cmd = cmd.getCode();
            this.cmdBody = cmdBody;
        }

        private byte[] encodeImpl() {
            ArrayList<Byte> bytes = new ArrayList<>(Coder.encodeVARUINT(src));
            bytes.addAll(Coder.encodeVARUINT(dst));
            bytes.addAll(Coder.encodeVARUINT(serial));
            bytes.add(devType);
            bytes.add(cmd);
            bytes.addAll(cmdBody.encode());


            bytes.add(Coder.calcCrc8(bytes, bytes.size() - 1));

            byte[] realBytes = new byte[bytes.size() + 1];
            realBytes[0] = (byte) ((byte) bytes.size() - 1);

            for (int i = 0; i < bytes.size(); i++) {
                realBytes[i + 1] = bytes.get(i);
            }

            return realBytes;
        }

        private String encode() {
            return Base64Helper.encodeBase64(encodeImpl());
        }

        private static String encode(List<Payload<?>> payloads) {
            ArrayList<Byte> bytes = new ArrayList<>();
            for (Payload<?> p : payloads) {
                for (byte b : p.encodeImpl()) {
                    bytes.add(b);
                }
            }
            byte[] realBytes = new byte[bytes.size()];
            for (int i = 0; i < bytes.size(); i++) {
                realBytes[i] = bytes.get(i);
            }
            return Base64Helper.encodeBase64(realBytes);
        }

        @Override
        public String toString() {
            return "[\n" +
                    "    {\n" +
                    "        \"payload\": {\n" +
                    "            \"src\": " + src + ",\n" +
                    "            \"dst\": " + dst +  ",\n" +
                    "            \"serial\": " + serial + ",\n" +
                    "            \"dev_type\": " + devType + ",\n" +
                    "            \"cmd\": " + cmd + ",\n" +
                    "            \"cmd_body\": {\n" +
                    "                 " + cmdBody.toString() + "\n" +
                    "            }\n" +
                    "        },\n" +
                    "    }\n" +
                    "]";
        }
    }








    public solution(String url, long srcName) {
        this.srcName = srcName;
        this.url = url;
    }


    //Стартовый запрос приветствия
    public void runWhoIsHere() throws IOException {
        sendPayload(
                new Payload<>(
                        srcName,
                        BROADCAST_DST,
                        counterSerial.inc(),
                        DevType.SMART_HUB,
                        CMD.WHO_IS_HERE,
                        new Name(MY_NAME)
                )
        );
    }


    private List<Payload<?>> getAllNewPayloads() throws IOException {
        List<Payload<?>> payloads = Coder.decodeAllRequests(readAllRequests());

        //null возвращается только при невозможности декодировать входящие данные
        if (payloads == null) {
            //В этом случае выполняем следующий пост запрос... (а может надо завершиться - хз)
            return null;
        }

        for (Payload<?> p : payloads) {
            if (CMD.getVal(p.cmd) == CMD.TICK) {
                curTime = ((Timer) p.cmdBody).time;
                if (startTime == 0L) {
                    startTime = curTime;
                }
            }
        }

        if (curTime - startTime > 300L) {
            flagSync = false;
        }
        return payloads;
    }

    @SuppressWarnings("InfiniteLoopStatement")
    public void standardWorking() throws IOException {
        while (true) {

            List<Payload<?>> payloads = getAllNewPayloads();
            List<Payload<?>> newQuest = new ArrayList<>();

            //на этом этапе payloads null -
            // если возникла ошибка декодирования (что-то закодировано в base64 неверно или там невозможные сообщения отправлены)
            if (payloads == null) {
                continue;
            }

            payloads.forEach(req -> {
                DevType devType = DevType.getVal(req.devType);
                CMD cmd = CMD.getVal(req.cmd);

                switch (cmd) {
                    case WHO_IS_HERE -> {
                        LAN_Structure.addNewDevice(req);
                        newQuest.add(
                                new Payload<>(
                                        srcName,
                                        BROADCAST_DST,
                                        counterSerial.inc(),
                                        DevType.SMART_HUB,
                                        CMD.I_AM_HERE,
                                        new Name(MY_NAME)
                                )
                        );
                        if (devType != DevType.CLOCK) { //Запрашиваем статус после приветствия
                            LAN_Structure.addNewDevice(req);
                            newQuest.add(
                                    new Payload<>(
                                            srcName,
                                            req.src,
                                            counterSerial.inc(),
                                            devType,
                                            CMD.GET_STATUS,
                                            new Empty()
                                    )
                            );
                        }
                    }
                    case I_AM_HERE -> {
                        if (devType != DevType.CLOCK && flagSync) { //Запрашиваем статус после приветствия (если меньше 300с прошло)
                            LAN_Structure.addNewDevice(req);
                            newQuest.add(
                                    new Payload<>(
                                            srcName,
                                            req.src,
                                            counterSerial.inc(),
                                            devType,
                                            CMD.GET_STATUS,
                                            new Empty()
                                    )
                            );
                        }
                    }
                    case STATUS -> {
                        if (LAN_Structure.workingDev(req.src)) {
                            switch (devType) {
                                case ENV_SENSOR -> {
                                    EnvSensor envSensor = (EnvSensor) LAN_Structure.mapAdrToD.get(req.src).cmdBody;
                                    int[] sensors = get4BitMask(envSensor.sensors); //4
                                    Trigger[] triggers = envSensor.triggers; //
                                    long[] values = ((Varuints) req.cmdBody).values;


                                    int s = 3;
                                    for (long value : values) {
                                        while (sensors[s] != 1) {
                                            s--;
                                        }
                                        for (Trigger curTr : triggers) {
                                            if (curTr.index != (3 - s)) {
                                                continue;
                                            }

                                            boolean flag = (curTr.type == 0 && value < curTr.value) ||
                                                    (curTr.type == 1 && value > curTr.value);

                                            if (flag && LAN_Structure.workingDev(curTr.name)) {
                                                Payload<?> wDevice = LAN_Structure.mapNameToD.get(curTr.name);
                                                newQuest.add(
                                                        new Payload<>(
                                                                srcName,
                                                                wDevice.src,
                                                                counterSerial.inc(),
                                                                DevType.getVal(wDevice.devType),
                                                                CMD.SET_STATUS,
                                                                new OneByteDevice((byte) curTr.flag)
                                                        )
                                                );
                                            }
                                        }
                                        s--;
                                    }
                                }
                                case SWITCH -> {
                                    Payload<?> switcher = LAN_Structure.mapAdrToD.get(req.src);
                                    for (String name : ((Strings) switcher.cmdBody).devNames) {
                                        if (LAN_Structure.workingDev(name)) {
                                            Payload<?> wDevice = LAN_Structure.mapNameToD.get(name);
                                            newQuest.add(
                                                    new Payload<>(
                                                            srcName,
                                                            wDevice.src,
                                                            counterSerial.inc(),
                                                            DevType.getVal(wDevice.devType),
                                                            CMD.SET_STATUS,
                                                            new OneByteDevice(((OneByteDevice) req.cmdBody).bfl)
                                                    )
                                            );
                                        }
                                    }
                                }
                                case LAMP, SOCKET, CLOCK -> {
                                    //read (вроде бы ничего не делаем)
                                }
                            }

                            //удаляем прошлый запрос (на который мы получили ответ если таковой был)
                            LAN_Structure.setRequests.entrySet().removeIf(pair ->
                                    (pair.getValue().getValue().cmd == CMD.GET_STATUS.getCode() ||
                                            pair.getValue().getValue().cmd == CMD.SET_STATUS.getCode()) &&
                                            pair.getValue().getValue().dst == req.src);
                        }
                    }
                    case TICK -> {
                        long tmp = ((Timer) req.cmdBody).time;
                        curTime = Math.max(tmp, curTime);
                    }
                }
            });

            //отправляем новые запросы
            sendRequest(Payload.encode(newQuest));

            //добавляем в неактивные устройства то что просрочило время
            LAN_Structure.setRequests.entrySet().removeIf(longEntryEntry -> {
                boolean fl = curTime - longEntryEntry.getValue().getKey() > 300L;
                if (fl && (longEntryEntry.getValue().getValue().cmd == CMD.GET_STATUS.code ||
                        longEntryEntry.getValue().getValue().cmd == CMD.SET_STATUS.code)) {
                    LAN_Structure.offDevices.add(longEntryEntry.getKey());
                }
                return fl;
            });

            //запоминаем только что отосланные запросы
            newQuest.forEach(p -> {
//                System.out.println("New data");
                LAN_Structure.setRequests.put(p.src, Map.entry(curTime, p));
            });

        }
    }

    public void start() {
        try {
            runWhoIsHere();
            standardWorking();
        } catch (IOException e) {
            System.exit(ERROR_EXIT_CODE);
        }
    }

    public static void main(String[] args) {
        if (args == null) {
            System.err.println("CMD arguments is null");
            return;
        }
        if (args.length != 2) {
            System.err.println("Incorrect length of cmd arguments (must me only 2)");
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Error because cmd array contains nulls");
        }


        final long srcName;
        try {
            srcName = Long.parseLong(args[1], 16);
        } catch (NumberFormatException e) {
            System.err.println("Second argument must be 14-bit number");
            System.err.println(e.getMessage());
            return;
        }

        solution smartHome = new solution(args[0], srcName);
        smartHome.start();

//        Пример вызова функции для ручной распаковки любого множества пакетов.
//        Coder.decodeAllRequests("EQIBBgIEBKUB4AfUjgaMjfILrw");
    }

    private void sendPayload(Payload<?> req) throws IOException {
        sendRequest(req.encode());
    }

    private void sendRequest(String mes) throws IOException {
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setDoOutput(true);
            connection.setRequestMethod("POST");
        } catch (IOException e) {
            System.exit(ERROR_EXIT_CODE);
        }

        try (OutputStreamWriter out = new OutputStreamWriter(connection.getOutputStream(), CHARSET)) {
            out.write(mes);
        }
        checkResponseCode(connection.getResponseCode());
    }

    private static void checkResponseCode(int code) {
        if (code != 200 && code != 204) {
            System.exit(ERROR_EXIT_CODE);
        }
        if (code == 204) {
            System.exit(DEFAULT_EXIT_CODE);
        }
    }

    private String readAllRequests() throws IOException {
        StringBuilder sb = new StringBuilder();
        try (BufferedReader input = new BufferedReader(new InputStreamReader(connection.getInputStream(), CHARSET))) {
            String str;
            while((str = input.readLine()) != null) {
                sb.append(str);
            }
        }
        return sb.toString();
    }

    private static List<Byte> byteArrayToList(byte[] byteArr) {
        List<Byte> bytes = new ArrayList<>();
        for (byte b : byteArr) {
            bytes.add(b);
        }
        return bytes;
    }

    private static int[] get4BitMask(byte b) {
        StringBuilder decodeByte = new StringBuilder(Integer.toBinaryString(b));
        while (decodeByte.length() < 4) {
            decodeByte.insert(0, "0");
        }
        int[] arr = new int[4];
        for (int i = 0; i < 4; i++) {
            arr[i] = Integer.parseInt(Character.toString(decodeByte.charAt(i)));
        }
        return arr;
    }



}
