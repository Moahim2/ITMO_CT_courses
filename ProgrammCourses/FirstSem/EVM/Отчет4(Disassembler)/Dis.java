package hw4;

import java.io.*;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.HashMap;

public class Dis {

    public static void main(String[] args) {

        long e_Shoff, e_shentsize, e_shnum; // оффсет на таблицу заголовков секций, длина секции, кол-во секций
        String TEXT = ".text", SYM_TAB = ".symtab", STR_TAB = ".strtab"; // имена соответствующих секций в e_shstrndx
        long addresProg_Bits = -1, offsetProg_Bits = -1, sizeProg_Bits = -1;
        long offsetSymTab = -1, sizeSymTab = -1;
        long offsetStrTab = -1, sizeStrTab = -1;
        long e_shstrndx, offsetE_shstrndx, nameE_shstrndx;

        byte[] progBits;
        ArrayList<String> binaryCommands = new ArrayList<>();
        String[] comands;
        String[] marks;
        ArrayList<byte[]> binaryMark = new ArrayList<>();
        byte[] symtabBits;
        byte[] strtabBits;

        HashMap<Long, String> mapFuncMarks;
        HashMap<Long, String> jumps;
        HashMap<Long, Long> placesJumps;

        try {
            Path nameFileInput = FileSystems.getDefault().getPath(args[0]);
            String nameFileOutput = args[1];
            byte[] in;
            try {
                in = Files.readAllBytes(nameFileInput); // считывание всего файла, и проверка его корректности
                if (in[0] != 0x7f || in[1] != 0x45 || in[2] != 0x4c || in[3] != 0x46) {
                    System.out.println("This file isn't ELF");
                    System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
                    return;
                }
                if (in[4] != 0x01) {
                    System.out.println("This file is ELF, but it isn't 32-bits");
                    System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
                    return;
                }
                if (in[5] != 0x01) {
                    System.out.println("This file is 32-bits ELF, but it isn't LittleEndian");
                    System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
                    return;
                }
                if (Byte.toUnsignedInt(in[18]) != 0xF3) {
                    System.out.println("This file is 32-bits LittleEndian ELF, but it isn't RISC-V");
                    System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
                    return;
                }
                e_shentsize = Byte.toUnsignedInt(in[0x2E]) + Byte.toUnsignedInt(in[0x2F]) * 256; //считываем константы
                e_Shoff = Byte.toUnsignedInt(in[32]) + Byte.toUnsignedInt(in[33]) * 256 +
                        Byte.toUnsignedInt(in[34]) * 256 * 256 + (long) Byte.toUnsignedInt(in[35]) * 256 * 256 * 256;
                e_shnum = Byte.toUnsignedInt(in[0x30]) + Byte.toUnsignedInt(in[0x31]) * 256;
                e_shstrndx = (Byte.toUnsignedInt(in[50]) + Byte.toUnsignedInt(in[51]) * 256);
                nameE_shstrndx = e_Shoff + (e_shstrndx) * e_shentsize;

                offsetE_shstrndx = Byte.toUnsignedInt(in[(int) nameE_shstrndx + 16]) +
                        Byte.toUnsignedInt(in[(int) nameE_shstrndx + 17])* 256 +
                        Byte.toUnsignedInt(in[(int) nameE_shstrndx + 18]) * 256 * 256 +
                        (long) Byte.toUnsignedInt(in[(int) nameE_shstrndx + 19]) * 256 * 256 * 256;

                for (long i = e_Shoff; i < e_Shoff + e_shentsize * e_shnum; i += e_shentsize) { // считываем координаты секций
                    long name = Byte.toUnsignedInt(in[(int) i]) + Byte.toUnsignedInt(in[(int) i + 1]) * 256 +
                            Byte.toUnsignedInt(in[(int) i + 2]) * 256 * 256 + (long) Byte.toUnsignedInt(in[(int) i + 3])
                            * 256 * 256 * 256;
                    long type = Byte.toUnsignedInt(in[(int) i + 4]) + Byte.toUnsignedInt(in[(int) i + 5]) * 256 +
                            Byte.toUnsignedInt(in[(int) i + 6]) * 256 * 256 + (long) Byte.toUnsignedInt(in[(int) i + 7])
                            * 256 * 256 * 256;

                    ArrayList<Byte> chars = new ArrayList<>(); //проверка имени секции
                    String stringName = "";
                    long thisSectionOffset = offsetE_shstrndx + name;
                    while (in[(int) thisSectionOffset] != 0) {
                        chars.add(in[(int) thisSectionOffset]);
                        thisSectionOffset++;
                    }
                    if (chars.size() != 0) {
                        byte[] charsStringName = new byte[chars.size()];
                        for (int j = 0; j < chars.size(); j++) {
                            charsStringName[j] = chars.get(j);
                        }
                        stringName = new String(charsStringName);
                    }
                    long addres = Byte.toUnsignedInt(in[(int) i + 12]) + Byte.toUnsignedInt(in[(int) i + 13])
                            * 256 + Byte.toUnsignedInt(in[(int) i + 14]) * 256 * 256 +
                            (long) Byte.toUnsignedInt(in[(int) i + 15]) * 256 * 256 * 256;
                    long offset = Byte.toUnsignedInt(in[(int) i + 16]) + Byte.toUnsignedInt(in[(int) i + 17])
                            * 256 + Byte.toUnsignedInt(in[(int) i + 18]) * 256 * 256 +
                            (long) Byte.toUnsignedInt(in[(int) i + 19]) * 256 * 256 * 256;
                    long size = Byte.toUnsignedInt(in[(int) i + 20]) + Byte.toUnsignedInt(in[(int) i + 21])
                            * 256 + Byte.toUnsignedInt(in[(int) i + 22]) * 256 * 256 +
                            (long) Byte.toUnsignedInt(in[(int) i + 23]) * 256 * 256 * 256;
                    if (type == 0x1 && stringName.equals(TEXT)) {
                        addresProg_Bits = addres;
                        offsetProg_Bits = offset;
                        sizeProg_Bits = size;
                    } else if (type == 0x02 && stringName.equals(SYM_TAB)) {
                        offsetSymTab = offset;
                        sizeSymTab = size;
                    } else if (type == 0x03 && stringName.equals(STR_TAB)) {
                        offsetStrTab = offset;
                        sizeStrTab = size;
                    }
                }

                progBits = new byte[(int) sizeProg_Bits];
                for (long i = offsetProg_Bits; i < offsetProg_Bits + sizeProg_Bits; i++) {
                    progBits[(int) (i - offsetProg_Bits)] = in[(int) i];
                }
                for (int i = 0; i < progBits.length; i += 2) { //перевод команд в двоичные строки
                    String lastB = "0" + "0" + Integer.toBinaryString(Byte.toUnsignedInt(progBits[i]));
                    StringBuilder sb = new StringBuilder();
                    if (lastB.substring(lastB.length() - 2, lastB.length()).equals("11")) { //RVC  or RVI
                        for (int j = i + 3; j >= i; j--) { //RVI
                            String binary = Integer.toBinaryString(Byte.toUnsignedInt(progBits[j]));
                            if (binary.length() < 8) {
                                sb.append("0".repeat(8 - binary.length()));
                            }
                            sb.append(binary);
                        }
                        binaryCommands.add(sb.toString());
                        i += 2;
                    } else {
                        for (int j = i + 1; j >= i; j--) { //RVC
                            String binary = Integer.toBinaryString(Byte.toUnsignedInt(progBits[j]));
                            if (binary.length() < 8) {
                                sb.append("0".repeat(8 - binary.length()));
                            }
                            sb.append(binary);
                        }
                        binaryCommands.add(sb.toString());
                    }
                }
                ProcCommand procCommand = new ProcCommand(binaryCommands, addresProg_Bits); //обработка команд
                comands =  procCommand.disCommand();
                jumps = procCommand.getJumps(); //передача мапы прыжков
                placesJumps = procCommand.getPlacesJumps();//

                symtabBits = new byte[(int) sizeSymTab];
                for (long i = offsetSymTab; i < offsetSymTab + sizeSymTab; i++) {
                    symtabBits[(int) (i - offsetSymTab)] = in[(int) i];
                }
                for (int i = 0; i < symtabBits.length; i += 16) {
                    byte[] mark = new byte[16];
                    for (int j = i; j < i + 16; j++) {
                        mark[j - i] = symtabBits[j];
                    }
                    binaryMark.add(mark);
                }
                strtabBits = new byte[(int) sizeStrTab];
                for (long i = offsetStrTab; i < offsetStrTab + sizeStrTab; i++) {
                    strtabBits[(int) (i - offsetStrTab)] = in[(int) i];
                }
                ParseSymTab parseSymTab = new ParseSymTab(binaryMark, strtabBits); // обработка симтаба
                marks = parseSymTab.disSymTab();
                mapFuncMarks = parseSymTab.getMapFuncMarks(); //передача мапы функций
            } catch (IOException e) {
                System.out.println("Cannot read: " + e.getMessage());
                System.out.println("You must enter the data in the format: " +
                        "hw4.Dis <имя_входного_elf_файла> <имя_выходного_файла>");
                System.out.println("And yor file must be correct 32-bits LittleEndian ELF on RISC-V");
                return;
            }
            try { //вывод (сначала команды потом симтаб)
                try (BufferedWriter output = new BufferedWriter(
                        new OutputStreamWriter(
                                new FileOutputStream(nameFileOutput)
                        )
                )) {
                    output.write(String.format("%s\n", TEXT));
                    long counter = addresProg_Bits;
                    for (int i = 0; i < comands.length; i++) { // вывод команд
                        if (mapFuncMarks.containsKey(counter)) { // добавление меток функций
                            String func = mapFuncMarks.get(counter);
                            if (placesJumps.containsKey(counter)) { //добавление метки того "куда попадает прыжок" (если команда прыгает)
                                if (mapFuncMarks.containsKey(placesJumps.get(counter))) { // если место прыжка - FUNC
                                    output.write(String.format("%08x %10s: %s %s\n", counter, func, comands[i],
                                            mapFuncMarks.get(placesJumps.get(counter))));
                                } else { // если место прыжка не FUNC
                                    output.write(String.format("%08x %10s: %s LOC_%05x\n", counter, func, comands[i],
                                            placesJumps.get(counter)));
                                }
                            } else {
                                output.write(String.format("%08x %10s: %s\n", counter, func, comands[i]));
                            }
                        } else if (jumps.containsKey(counter)) { // добавление новых меток "на прыжки"
                            String ourMark = jumps.get(counter);
                            if (placesJumps.containsKey(counter)) {
                                if (mapFuncMarks.containsKey(placesJumps.get(counter))) {
                                    output.write(String.format("%08x %10s: %s %s\n", counter, ourMark, comands[i],
                                            mapFuncMarks.get(placesJumps.get(counter))));
                                } else {
                                    output.write(String.format("%08x %10s: %s LOC_%05x\n", counter, ourMark, comands[i],
                                            placesJumps.get(counter)));
                                }
                            } else {
                                output.write(String.format("%08x %10s: %s\n", counter, ourMark, comands[i]));
                            }
                        } else { //вывод если прыжков и функций нет
                            if (placesJumps.containsKey(counter)) {
                                if (mapFuncMarks.containsKey(placesJumps.get(counter))) {
                                    output.write(String.format("%08x %10s %s %s\n", counter, "", comands[i],
                                            mapFuncMarks.get(placesJumps.get(counter))));
                                } else {
                                    output.write(String.format("%08x %10s %s LOC_%05x\n", counter, "", comands[i],
                                            placesJumps.get(counter)));
                                }
                            } else {
                                output.write(String.format("%08x %10s %s\n", counter, "", comands[i]));
                            }
                        }
                        if (comands[i].charAt(0) == 'C') {
                            counter += 2; // если 16-битная команда
                        } else {
                            counter += 4; // если 32-битная
                        }
                    }
                    output.newLine();

                    output.write(String.format("%s\n", SYM_TAB));
                    output.write(String.format("%s %-15s %7s %-8s %-8s %-8s %6s %s\n", "Symbol", "Value",
                            "Size", "Type", "Bind", "Vis", "Index", "Name"));
                    for (int i = 0; i < binaryMark.size(); i++) {
                        output.write(marks[i]);
                    }
                    output.close();
                }
            } catch (FileNotFoundException e) {
                System.out.println(nameFileOutput + "not found" + e.getMessage());
                System.out.println("You must enter the data in the format: " +
                        "hw4.Dis <имя_входного_elf_файла> <имя_выходного_файла>");
                return;
            } catch (IOException e) {
                System.out.println("Cannot write in output file: " + e.getMessage());
                System.out.println("You must enter the data in the format: " +
                        "hw4.Dis <имя_входного_elf_файла> <имя_выходного_файла>");
                return;
            }
        } catch (IllegalArgumentException e) {
            System.out.println("File input incorrect" + e.getMessage());
            System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
            return;
        } catch (IndexOutOfBoundsException e) {
            System.out.println("File input incorrect" + e.getMessage());
            System.out.println("Input file must be correct 32-bits LittleEndian ELF on RISC-V");
        }
    }
}
