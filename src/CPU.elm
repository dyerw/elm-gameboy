module CPU exposing (..)

import Binary exposing (Bit(..), BitIndex(..), Byte, zeroByte)


type EightBitRegisterName
    = A -- the accumulator register
    | B -- registers B thru L are general purpose
    | C
    | D
    | E
    | H
    | L
    | F -- status register


type SixteenBitRegisterName
    = SP -- stack pointer
    | PC -- program counter


type RegisterName
    = EightBitRegisterName
    | SixteenBitRegisterName


type alias RegisterState =
    { a : Byte
    , b : Byte
    , c : Byte
    , d : Byte
    , e : Byte
    , h : Byte
    , l : Byte
    , f : Byte
    , sp : ( Byte, Byte )
    , pc : ( Byte, Byte )
    }


setEightBitRegister : EightBitRegisterName -> Byte -> RegisterState -> RegisterState
setEightBitRegister eightBitRegister byte registerState =
    case eightBitRegister of
        A ->
            { registerState | a = byte }

        B ->
            { registerState | b = byte }

        C ->
            { registerState | c = byte }

        D ->
            { registerState | d = byte }

        E ->
            { registerState | e = byte }

        H ->
            { registerState | h = byte }

        L ->
            { registerState | l = byte }

        F ->
            { registerState | f = byte }


setSixteenbitRegister : SixteenBitRegisterName -> ( Byte, Byte ) -> RegisterState -> RegisterState
setSixteenbitRegister sixteenBitRegisterName bytes registerState =
    case sixteenBitRegisterName of
        SP ->
            { registerState | sp = bytes }

        PC ->
            { registerState | pc = bytes }


initialRegisterState : RegisterState
initialRegisterState =
    { a = zeroByte
    , b = zeroByte
    , c = zeroByte
    , d = zeroByte
    , e = zeroByte
    , h = zeroByte
    , l = zeroByte
    , f = zeroByte
    , sp = ( zeroByte, zeroByte )
    , pc = ( zeroByte, zeroByte )
    }


type Flag
    = NonZero
    | Zero
    | NonCarry
    | Carry


type RegisterOperand
    = Single RegisterName
    | Double EightBitRegisterName EightBitRegisterName


type Instruction
    = NOP
    | STOP
    | LDValue RegisterOperand Byte
    | LDRegister RegisterOperand RegisterOperand
    | LDD RegisterOperand RegisterOperand
    | PUSH RegisterOperand
    | POP RegisterOperand
    | ADD RegisterOperand
    | ADC RegisterOperand
    | SUB RegisterOperand
    | SBC RegisterOperand
    | AND RegisterOperand
    | OR RegisterOperand
    | XOR RegisterOperand
    | CP RegisterOperand
    | INC RegisterOperand
    | DEC RegisterOperand
    | ADDSP Byte
    | SWAP RegisterOperand
    | DAA
    | CPL
    | CCF
    | SCF
    | HALT
    | DI
    | EI
    | RLCA
    | RLA
    | RRCA
    | RRA
    | RLC RegisterOperand
    | RL RegisterOperand
    | RRC RegisterOperand
    | RR RegisterOperand
    | SLA RegisterOperand
    | SRA RegisterOperand
    | SRL RegisterOperand
    | BIT BitIndex RegisterOperand
    | SET BitIndex RegisterOperand
    | RES BitIndex RegisterOperand
    | JP Byte Byte
    | JPFlag Flag Byte Byte
    | JPHL
    | JR Byte
    | JRFlag Flag Byte
    | CALL Byte Byte
    | CALLFlag Flag Byte Byte
    | RST Byte
    | RET
    | RETFlag Flag
    | RETI


execute : Instruction -> RegisterState -> RegisterState
execute instruction registerState =
    initialRegisterState
