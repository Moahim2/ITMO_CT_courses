package hw4;

import java.util.ArrayList;
import java.util.HashMap;

public class ParseSymTab {
    private final ArrayList<byte[]> binaryMarks;
    private final byte[] strTabBits;
    private final HashMap<Long, String> mapFuncMarks = new HashMap<>();


    public ParseSymTab(ArrayList<byte[]> binaryMarks, byte[] strTabBits) {
        this.binaryMarks = binaryMarks;
        this.strTabBits = strTabBits;
    }

    private String getName(long offset) { //обрабатываем имя метки
        String name = "";
        int j = (int) offset;
        ArrayList<Byte> bytesforName = new ArrayList<>();
        while (Byte.toUnsignedInt(strTabBits[j]) != 0) { //считываем имя метки
            bytesforName.add(strTabBits[j]);
            j++;
        }
        if (bytesforName.size() != 0) { //переводим имя метки в символ
            byte[] stringName = new byte[bytesforName.size()];
            for (int k = 0; k < stringName.length; k++) {
                stringName[k] = bytesforName.get(k);
            }
            name = new String(stringName);
        }
        return name;
    }



    public String[] disSymTab() throws IndexOutOfBoundsException { // обрабатываем все части меток
        String[] disassemblersMarks = new String[binaryMarks.size()];

        int count = 0;
        for (int i = 0; i < binaryMarks.size(); i++) {
            byte[] mark = binaryMarks.get(i);
            long offset = Byte.toUnsignedInt(mark[0]) + Byte.toUnsignedInt(mark[1]) * 256 +
                    Byte.toUnsignedInt(mark[2]) * 256 * 256 + (long) Byte.toUnsignedInt(mark[3]) * 256 * 256 * 256;
            String name = getName(offset);
            long value = Byte.toUnsignedInt(mark[4]) + Byte.toUnsignedInt(mark[5]) * 256 +
                    Byte.toUnsignedInt(mark[6]) * 256 * 256 + (long) Byte.toUnsignedInt(mark[7]) * 256 * 256 * 256;
            long size = Byte.toUnsignedInt(mark[8]) + Byte.toUnsignedInt(mark[9]) * 256 +
                    Byte.toUnsignedInt(mark[10]) * 256 * 256 + (long) Byte.toUnsignedInt(mark[11]) * 256 * 256 * 256;
            int typeMark = Byte.toUnsignedInt(mark[12]);
            String bind = "", type = "", vis = "";
            switch (typeMark / 16) { // bind
                case 0:
                    bind = "LOCAL";
                    break;
                case 1:
                    bind = "GLOBAL";
                    break;
                case 2:
                    bind = "WEAK";
                    break;
                case 10:
                case 11:
                case 12:
                    bind = "OS";
                    break;
                case 13:
                case 14:
                case 15:
                    bind = "PROC";
                    break;
                default:
                    bind = "Unknown";
            }

            switch (typeMark % 16) { //type
                case 0:
                    type = "NOTYPE";
                    break;
                case 1:
                    type = "OBJECT";
                    break;
                case 2:
                    type = "FUNC";
                    break;
                case 3:
                    type = "SECTION";
                    break;
                case 4:
                    type = "FILE";
                    break;
                case 5:
                    type = "COMMON";
                    break;
                case 6:
                    type = "TLS";
                    break;
                case 10:
                case 11:
                case 12:
                    type = "OS";
                    break;
                case 13:
                    type = "_SPARC_REGISTER";
                    break;
                case 14:
                case 15:
                    type = "PROC";
                    break;
                default:
                    type = "Unknown";
            }
            int visibility = Byte.toUnsignedInt(mark[13]);
            switch (visibility) { //vis
                case 0:
                    vis = "DEFAULT";
                    break;
                case 1:
                    vis = "INTERNAL";
                    break;
                case 2:
                    vis = "HIDDEN";
                    break;
                case 3:
                    vis = "PROTECTED";
                    break;
                default:
                    vis = "Unknown";
            }
            long index = Byte.toUnsignedInt(mark[14]) + Byte.toUnsignedInt(mark[15]) * 256;
            String sectionOnIndex = "";
            if (index == 0) { // cоответствие секции по индексу (вложенные промежутки названы одним именем)
                sectionOnIndex = "UNDEF";
            } else if (index == 0xff00) {
                sectionOnIndex = "BEFORE";
            } else if (index == 0xff01) {
                sectionOnIndex = "AFTER";
            } else if (index == 0xfff1) {
                sectionOnIndex = "ABS";
            } else if (index == 0xfff2) {
                sectionOnIndex = "COMMON";
            } else if (index == 0xffff) {
                sectionOnIndex = "XINDEX";
            } else if (index <= 0xff3f && index >= 0xff20) {
                sectionOnIndex = "OS";
            } else if (index <= 0xff1f && index >= 0xff00) {
                sectionOnIndex = "PROC";
            } else if (index <= 0xffff && index >= 0xff00) {
                sectionOnIndex = "RESERVE";
            }
            if (sectionOnIndex.isEmpty()) {
                sectionOnIndex = Long.toString(index);
            }
            if (type.equals("FUNC")) { //запись в мапу меток, чтобы потом приписать в вывод команд
                if (name.isEmpty()) {
                    mapFuncMarks.put(value, String.format("LOC_%05x", value));
                } else {
                    mapFuncMarks.put(value, name);
                }
            }
            disassemblersMarks[i] = String.format("[%4d] 0x%-15X %5d %-8s %-8s %-8s %6s %s\n", count, value, size,
                    type, bind, vis, sectionOnIndex, name);
            count++;
        }
        return disassemblersMarks;
    }

    public HashMap<Long, String> getMapFuncMarks() throws IndexOutOfBoundsException { //передаем мапу меток-функций
        return mapFuncMarks;
    }
}
