package hw4;

import java.util.ArrayList;
import java.util.HashMap;

public class ProcCommand {
    private long thisAddresProg_Bits;

    private final ArrayList<String> binaryCommands;
    private final HashMap<Long, String> jumps = new HashMap<>();
    private final HashMap<Long, Long> placesJumps = new HashMap<>();

    final HashMap<String, String> registerNames = new HashMap<>() {{ // обычные регистры
        put("00000", "zero"); put("00001", "ra"); put("00010", "sp");
        put("00011", "gp"); put("00100", "tp"); put("00101", "t0");
        put("00110", "t1"); put("00111", "t2"); put("01000", "s0");
        put("01001", "s1"); put("01010", "a0"); put("01011", "a1");
        put("01100", "a2"); put("01101", "a3"); put("01110", "a4");
        put("01111", "a5"); put("10000", "a6"); put("10001", "a7");
        put("10010", "s2"); put("10011", "s3"); put("10100", "s4");
        put("10101", "s5"); put("10110", "s6"); put("10111", "s7");
        put("11000", "s8"); put("11001", "s9"); put("11010", "s10");
        put("11011", "s11"); put("11100", "t3"); put("11101", "t4");
        put("11110", "t5"); put("11111", "t6");
    }};

    final HashMap<Integer, String> registersCSR = new HashMap<>() {{ //машинные регистры
        put(0x001, "fflags"); put(0x002, "frm"); put(0x003, "fcsr");
        put(0xC00, "cycle"); put(0xC01, "time"); put(0xC02, "instret");
        put(0xC80, "cycleh"); put(0xC81, "timeh"); put(0xC82, "instreth");
        put(0x100, "sstatus"); put(0x104, "sie"); put(0x105, "stvec");
        put(0x106, "scounteren"); put(0x10A, "senvcfg"); put(0x140, "sscratch");
        put(0x141, "sepc"); put(0x142, "scause"); put(0x143, "stval");
        put(0x144, "sip"); put(0x180, "satp"); put(0x5A8, "scontext");
        put(0x600, "hstatus"); put(0x602, "hedeleg"); put(0x603, "hideleg");
        put(0x604, "hie"); put(0x606, "hcounteren"); put(0x607, "hgeie");
        put(0x643, "htval"); put(0x644, "hip"); put(0x645, "hvip");
        put(0x64A, "htinst"); put(0xE12, "hgeip"); put(0x60A, "henvcfg");
        put(0x61A, "henvcfgh"); put(0x680, "hgatp"); put(0x6A8, "hcontext");
        put(0x605, "htimedelta"); put(0x615, "htimedeltah"); put(0x200, "vsstatus");
        put(0x204, "vsie"); put(0x205, "vstvec"); put(0x240, "vsscratch");
        put(0x241, "vsepc"); put(0x242, "vscause"); put(0x243, "vstval");
        put(0x244, "vsip"); put(0x280, "vsatp"); put(0xF11, "mvendorid");
        put(0xF12, "marchid"); put(0xF13, "mimpid"); put(0xF14, "mhartid");
        put(0xF15, "mconfigptr"); put(0x300, "mstatus"); put(0x301, "misa");
        put(0x302, "medeleg"); put(0x303, "mideleg"); put(0x304, "mie");
        put(0x305, "mtvec"); put(0x306, "mcounteren"); put(0x310, "mstatush");
        put(0x340, "mscratch"); put(0x341, "mepc"); put(0x342, "mcause");
        put(0x343, "mtval"); put(0x344, "mip"); put(0x34A, "mtinst");
        put(0x34B, "mtval2"); put(0x30A, "menvcfg"); put(0x31A, "menvcfgh");
        put(0x747, "mseccfg"); put(0x757, "mseccfgh"); put(0xB00, "mcycle");
        put(0xB02, "minstret"); put(0xB80, "mcycleh"); put(0xB82, "minstreth");
        put(0x320, "mcountinhibit"); put(0x7A0, "tselect"); put(0x7A8, "mcontext");
        put(0x7B0, "dcsr"); put(0x7B1, "dpc"); put(0x7B2, "dscratch0"); put(0x7B3, "dscratch1");
        put(0x7A1, "tdata1"); put(0x7A2, "tdata2"); put(0x7A3, "tdata3");
    }};

    final HashMap<String, String> registersRVC = new HashMap<>() {{ //сокращенные регистры
        put("000", "s0"); put("001", "s1"); put("010", "a0");
        put("011", "a1"); put("100", "a2"); put("101", "a3");
        put("110", "a4"); put("111", "a5");
    }};

    final HashMap<String, String> typeR_and_RV32M = new HashMap<>() {{ //далее мапы разбиты на похожие типы команд
        put("0000000000", "ADD"); put("0100000000", "SUB");
        put("0000000001", "SLL"); put("0000000010", "SLT");
        put("0000000011", "SLTU");put("0000000100", "XOR");
        put("0000000101", "SRL"); put("0100000101", "SRA");
        put("0000000110", "OR");  put("0000000111", "AND");
        put("0000001000", "MUL"); put("0000001001", "MULH");
        put("0000001010", "MULHSU"); put("0000001011", "MULHU");
        put("0000001100", "DIV"); put("0000001101", "DIVU");
        put("0000001110", "REM"); put("0000001111", "REMU");
    }};

    final HashMap<String, String> typeI = new HashMap<>() {{
        put("000", "ADDI"); put("001", "SLTI");
        put("011", "SLTIU"); put("100", "XORI");
        put("110", "ORI"); put("111", "ANDI");
        put("0000000001", "SLLI"); put("0000000101", "SRLI");
        put("0100000101", "SRLI");
    }};

    final HashMap<String, String> typeS = new HashMap<>() {{
        put("000", "SB"); put("001", "SH"); put("010", "SW");
    }};

    final HashMap<String, String> typeL = new HashMap<>() {{
        put("000", "LB"); put("001", "LH"); put("010", "LW");
        put("100", "LBU"); put("101", "LHU");
    }};

    final HashMap<String, String> typeB = new HashMap<>() {{
        put("000", "BEQ"); put("001", "BNE"); put("100", "BLT");
        put("101", "BGE"); put("110", "BLTU"); put("111", "BGEU");
    }};

    final HashMap<String, String> typeCSR = new HashMap<>() {{
        put("001", "CSRRW"); put("010", "CSRRS"); put("011", "CSRRC");
        put("101", "CSRRWI"); put("110", "CSRRSI"); put("111", "CSRRCI");
    }};



    public ProcCommand(ArrayList<String> binaryCommands, long addresProg_Bits) {
        this.binaryCommands = binaryCommands;
        thisAddresProg_Bits = addresProg_Bits;
    }

    private int parseInAdditionTo2(String binaryString) { //перевод числа в дополнении до 2
        return Integer.parseUnsignedInt(binaryString.substring(1, binaryString.length()), 2) -
                (1 << (binaryString.length() - 1)) * Integer.parseInt(Character.toString(binaryString.charAt(0)));
    }

    private String convertR_andR32M(String com) { //далее ф-ции вида convert обрабатывают типы команд
        StringBuilder sb = new StringBuilder();
        sb.append(typeR_and_RV32M.get(com.substring(0, 7) + com.substring(17, 20))).append(" ");
        sb.append(registerNames.get(com.substring(20, 25))).append(",").append(" ");
        sb.append(registerNames.get(com.substring(12, 17))).append(",").append(" ");
        sb.append(registerNames.get(com.substring(7, 12)));
        if (!typeR_and_RV32M.containsKey(com.substring(0, 7) + com.substring(17, 20))) {
            return "unknown_command";
        }
        return sb.toString();
    }
//
    private String convertI(String com) {
        StringBuilder sb = new StringBuilder();
        String str = com.substring(0, 7) + com.substring(17, 20);
        if (str.equals("0000000001") || str.equals("0000000101") || str.equals("0100000101")) {
            sb.append(typeI.get(str)).append(" ");
            sb.append(registerNames.get(com.substring(20, 25))).append(",").append(" ");
            sb.append(registerNames.get(com.substring(12, 17))).append(",").append(" ");
            sb.append(Integer.parseUnsignedInt(com.substring(7, 12), 2));
        } else {
            if (!typeI.containsKey(com.substring(17, 20))) {
                return "unknown_command";
            }
            sb.append(typeI.get(com.substring(17, 20))).append(" ");
            sb.append(registerNames.get(com.substring(20, 25))).append(",").append(" ");
            sb.append(registerNames.get(com.substring(12, 17))).append(",").append(" ");
            sb.append(parseInAdditionTo2(com.substring(0, 12)));
        }
        return sb.toString();
    }
    private String convertS(String com) {
        if (!typeS.containsKey(com.substring(17, 20))) {
            return "unknown_command";
        }
        return typeS.get(com.substring(17, 20)) + "," + " " +
                registerNames.get(com.substring(7, 12)) + "," + " " +
                parseInAdditionTo2(com.substring(0, 7) + com.substring(20, 25)) +
                "(" + registerNames.get(com.substring(12, 17)) + ")";
    }

    private String convertL(String com) {
        if (!typeL.containsKey(com.substring(17, 20))) {
            return "unknown_command";
        }
        return typeL.get(com.substring(17, 20)) + " " +
                registerNames.get(com.substring(20, 25)) + "," + " " +
                parseInAdditionTo2(com.substring(0, 12)) + "(" +
                registerNames.get(com.substring(12, 17)) + ")";
    }

    private String conwertB(String com) {
        if (!typeB.containsKey(com.substring(17, 20))) {
            return "unknown_command";
        }
        long jump = parseInAdditionTo2(com.substring(0, 1) + com.substring(24, 25) + com.substring(1, 7) +
                com.substring(20, 24) + "0");
        jumps.put(thisAddresProg_Bits + jump, String.format("LOC_%05x", thisAddresProg_Bits + jump));
        placesJumps.put(thisAddresProg_Bits, thisAddresProg_Bits + jump);
        return typeB.get(com.substring(17, 20)) + " " +
                registerNames.get(com.substring(12, 17)) + "," + " " +
                registerNames.get(com.substring(7, 12)) + "," + " " + jump;
    }




    private String conwertCSR(String com) {
        StringBuilder sb = new StringBuilder();
        int number = Integer.parseUnsignedInt(com.substring(0, 12));
        sb.append(typeCSR.get(com.substring(17, 20))).append(" ");
        sb.append(registerNames.get(com.substring(20, 25))).append(",").append(" ");
        if (registersCSR.containsKey(number)) {
            sb.append(registersCSR.get(number));
        } else {
            if (number <= 0xC1F && number >= 0xC03) {
                sb.append("hpmcounter").append(number - 0xC00);
            } else if (number <= 0xC9F && number >= 0xC83) {
                sb.append("hpmcounter").append(number - 0xC80).append("h");
            } else if (number <= 0x3AF && number >= 0x3A0) {
                sb.append("pmpcfg").append(number - 0x3A0);
            } else if (number <= 0x3EF && number >= 0x3B0) {
                sb.append("pmpaddr").append(number - 0x3B0);
            } else if (number <= 0xB1F && number >= 0xB03) {
                sb.append("mhpmcounter").append(number - 0xB00);
            } else if (number <= 0xB9F && number >= 0xB83) {
                sb.append("mhpmcounter").append(number - 0xB80).append("h");
            } else if (number <= 0x33F && number >= 0x323) {
                sb.append("mhpmevent").append(number - 0x320);
            } else {
                return "unknown_command";
            }
        }
        sb.append(",").append(" ");
        if (com.charAt(17) == '0') {
            sb.append(registerNames.get(com.substring(12, 17)));
        } else {
            sb.append(Integer.parseUnsignedInt(com.substring(12, 17)));
        }
        return sb.toString();
    }


    private String conwertRVC10001(String com) {
        switch (com.substring(4, 6)) {
            case "00":
                return "C.SRLI" + " " + registersRVC.get(com.substring(6, 9)) + "," + " " +
                        Integer.parseUnsignedInt(com.charAt(3) + com.substring(9, 14), 2);
            case "01":
                return "C.SRAI" + " " + registersRVC.get(com.substring(6, 9)) + "," + " " +
                        Integer.parseUnsignedInt(com.charAt(3) + com.substring(9, 14), 2);
            case "10":
                return "C.ANDI" + " " + registersRVC.get(com.substring(6, 9)) + "," + " " +
                        parseInAdditionTo2(com.charAt(3) + com.substring(9, 14));
            case "11":
                if (com.charAt(3) == '0') {
                    StringBuilder sb = new StringBuilder();
                    switch (com.substring(9, 11)) {
                        case "00":
                            sb.append("C.SUB");
                            break;
                        case "01":
                            sb.append("C.XOR");
                            break;
                        case "10":
                            sb.append("C.OR");
                            break;
                        case "11":
                            sb.append("C.AND");
                            break;
                    }
                    sb.append(" ").append(registersRVC.get(com.substring(6, 9))).append(",").append(" ");
                    sb.append(registersRVC.get(com.substring(11, 14)));
                    return sb.toString();
                } else {
                    break;
                }
        }
        return "unknown_command";
    }

    private String conwertRVC10010(String com) {
        if (com.charAt(3) == '0') {
            if (com.substring(9, 14).equals("00000")) {
                return "C.JR" + " " + registerNames.get(com.substring(4, 9));
            } else {
                return "C.MV" + " " + registerNames.get(com.substring(4, 9)) + "," + " " +
                        registerNames.get(com.substring(9, 14));
            }
        } else {
            if (com.substring(9, 14).equals("00000")) {
                if (com.substring(4, 9).equals("00000")) {
                    return "C.EBREAK";
                } else {
                    return "C.JALR" + " " + registerNames.get(com.substring(4, 9));
                }
            } else {
                return "C.ADD" + " " + registerNames.get(com.substring(4, 9)) + "," + " " +
                        registerNames.get(com.substring(9, 14));
            }
        }
    }


    public String[] disCommand() throws IndexOutOfBoundsException { //обработка всех команд
        String[] disassemblersCom = new String[binaryCommands.size()];
        for (int i = 0; i < binaryCommands.size(); i++) {
            String command = binaryCommands.get(i);
            if (command.length() == 32) { //обработка 32-битных
                switch (command.substring(25, 32)) {
                    case "0110011":
                        disassemblersCom[i] = convertR_andR32M(command); //R32IR R32M
                        break;
                    case "0010011":
                        disassemblersCom[i] = convertI(command); //I
                        break;
                    case "0100011":
                        disassemblersCom[i] = convertS(command); // S
                        break;
                    case "0000011":
                        disassemblersCom[i] = convertL(command); // L
                        break;
                    case "1100011":
                        disassemblersCom[i] = conwertB(command); //B
                        break;
                    case "1110011":
                        if (command.substring(0, 25).equals("0".repeat(25))) { //Ebreak, Ecall
                            disassemblersCom[i] = "ECALL";
                        } else if (command.substring(0, 25).equals("0".repeat(11) + "1" + "0".repeat(13))) {
                            disassemblersCom[i] = "EBREAK";
                        } else {
                            disassemblersCom[i] = conwertCSR(command); //CSR Zicsr
                        }
                        break;
                    case "1100111":
                        disassemblersCom[i] = "JALR" + " " + registerNames.get(command.substring(20, 25)) + "," + " " +
                                parseInAdditionTo2(command.substring(0, 12)) + "(" +
                                registerNames.get(command.substring(12, 17)) + ")";
                        break;
                    case "1101111":
                        long jump = parseInAdditionTo2(command.charAt(0) + command.substring(12, 20) +
                                command.charAt(11) + command.substring(1, 11) + "0");
                        disassemblersCom[i] = "JAL" + " " + registerNames.get(command.substring(20, 25)) + "," + " " +
                                jump;
                        jumps.put(thisAddresProg_Bits + jump, String.format("LOC_%05x", thisAddresProg_Bits + jump));
                        placesJumps.put(thisAddresProg_Bits, thisAddresProg_Bits + jump);
                        break;
                    case "0110111":
                        disassemblersCom[i] = "LUI" + " " + registerNames.get(command.substring(20, 25)) + "," + " " +
                                parseInAdditionTo2(command.substring(0, 20)); // не дописываем нули
                        break;
                    case "0010111":
                        disassemblersCom[i] = "AUIPC" + " " + registerNames.get(command.substring(20, 25)) + "," + " " +
                                parseInAdditionTo2(command.substring(0, 20)); //не дописываем нули
                        break;
                    default:
                        disassemblersCom[i] = "unknown_command";
                        break;
                }
                thisAddresProg_Bits += 4;
            } else { //обработка 16-битных
                switch (command.substring(0, 3) + command.substring(14, 16)) {
                    case "10001":
                        disassemblersCom[i] = conwertRVC10001(command);
                        break;
                    case "00000":
                        disassemblersCom[i] = "C.ADDI4SPN" + " " + registersRVC.get(command.substring(11, 14)) + "," +
                                " " + "sp" + "," + " " + Integer.parseUnsignedInt(command.substring(5, 9) +
                                command.substring(3, 5) + command.charAt(10) + command.charAt(9) +
                                "0" + "0", 2);
                        break;
                    case "01000":
                        disassemblersCom[i] = "C.LW" + " " + registersRVC.get(command.substring(11, 14)) + "," + " " +
                                Integer.parseUnsignedInt(command.charAt(10) + command.substring(3, 6) +
                                        command.charAt(9) + "0" + "0", 2) + "(" + registersRVC.get(
                                                command.substring(6, 9)) + ")";
                        break;
                    case "11000":
                        disassemblersCom[i] = "C.SW" + " " + registersRVC.get(command.substring(11, 14)) + "," + " " +
                                Integer.parseUnsignedInt(command.charAt(10) + command.substring(3, 6) +
                                        command.charAt(9)+ "0" + "0", 2) + "(" +
                                registersRVC.get(command.substring(6, 9)) + ")";
                        break;
                    case "00001":
                        if (command.substring(4, 9).equals("00000")) {
                            disassemblersCom[i] = "C.NOP" + " "; ///???
                        } else {
                            disassemblersCom[i] = "C.ADDI" + " " + registerNames.get(command.substring(4, 9)) + "," +
                                    " ";
                        }
                        disassemblersCom[i] += parseInAdditionTo2(command.charAt(3) +
                                command.substring(9, 14));
                        break;
                    case "00101":
                        long jump = parseInAdditionTo2(command.charAt(3) +
                                command.substring(7, 8) + command.substring(5, 7) + command.charAt(9) +
                                command.charAt(8) + command.charAt(13) + command.charAt(4) +
                                command.substring(10, 13) + "0");
                        disassemblersCom[i] = "C.JAL" + " " + jump;
                        jumps.put(thisAddresProg_Bits + jump, String.format("LOC_%05x", thisAddresProg_Bits + jump));
                        placesJumps.put(thisAddresProg_Bits, thisAddresProg_Bits + jump);
                        break;
                    case "01001":
                        disassemblersCom[i] = "C.LI" + " " + registerNames.get(command.substring(4, 9)) + "," + " " +
                                parseInAdditionTo2(command.charAt(3) + command.substring(9, 14));
                        break;
                    case "01101":
                        if (registerNames.get(command.substring(4, 9)).equals("sp")) {
                            disassemblersCom[i] = "C.ADDI16SP" + " " + "sp" + "," + " " + parseInAdditionTo2(
                                    command.charAt(3) + command.substring(11, 13) +
                                            command.charAt(10) + command.charAt(13)
                                            + command.charAt(9) + "0".repeat(4));
                        } else {
                            disassemblersCom[i] = "C.LUI" + " " + registerNames.get(command.substring(4, 9)) + "," + " "
                                    + parseInAdditionTo2(command.charAt(3) + command.substring(9, 14));
                        }
                        break;
                    case "10101":
                        jump = parseInAdditionTo2(command.charAt(3) +
                                command.substring(7, 8) + command.substring(5, 7) + command.charAt(9) +
                                command.charAt(8) + command.charAt(13) + command.charAt(4) +
                                command.substring(10, 13) + "0");
                        disassemblersCom[i] = "C.J" + " " + jump;
                        jumps.put(thisAddresProg_Bits + jump, String.format("LOC_%05x", thisAddresProg_Bits + jump));
                        placesJumps.put(thisAddresProg_Bits, thisAddresProg_Bits + jump);
                        break;
                    case "11001":
                    case "11101":
                        if (command.substring(0, 3).equals("110")) {
                            disassemblersCom[i] = "C.BEQZ";
                        } else {
                            disassemblersCom[i] = "C.BNEZ";
                        }
                        jump = parseInAdditionTo2(command.charAt(3) + command.substring(9, 11) +
                                command.charAt(13) + command.substring(4, 6) +
                                command.substring(11, 13) + "0");
                        disassemblersCom[i] += " " + registersRVC.get(command.substring(6, 9)) + "," + " " + jump;
                        jumps.put(thisAddresProg_Bits + jump, String.format("LOC_%05x", thisAddresProg_Bits + jump));
                        placesJumps.put(thisAddresProg_Bits, thisAddresProg_Bits + jump);
                        break;
                    case "00010":
                        disassemblersCom[i] = "C.SLLI" + " " + registerNames.get(command.substring(4, 9)) + "," +
                                " " + Integer.parseUnsignedInt(command.charAt(3) + command.substring(9, 14),
                                2);
                        break;
                    case "01010":
                        disassemblersCom[i] = "C.LWSP" + " " + registerNames.get(command.substring(4, 9)) + "," + " " +
                                Integer.parseUnsignedInt(command.substring(12, 14) + command.charAt(3) +
                                        command.substring(9, 12) + "0" + "0", 2) + "(sp)";
                        break;
                    case "10010":
                        disassemblersCom[i] = conwertRVC10010(command);
                        break;
                    case "11010":
                        disassemblersCom[i] =  "C.SWSP" + " " + registerNames.get(command.substring(9, 14)) + ","
                                + " " + Integer.parseUnsignedInt(command.substring(7, 9) + command.substring(3, 7) +
                                "0" + "0", 2) + "(sp)";
                        break;
                    default:
                        disassemblersCom[i] = "unknown_command";
                        break;
                }
                thisAddresProg_Bits += 2;
            }
        }
        return disassemblersCom;
    }

    public HashMap<Long, String> getJumps() { //возвращаем мапу прыжков (ставим если на команду есть прыжок)
        return jumps;
    }

    public HashMap<Long, Long> getPlacesJumps() { //(ставим если с команды есть прыжок и куда он)
        return placesJumps;
    }
}
