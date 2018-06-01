module CPU exposing (..)

import Binary
    exposing
        ( Bit(..)
        , BitIndex(..)
        , Byte
        , Word
        , zeroByte
        )
import MMU exposing (..)


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
    | BC -- BC thru HL are "virtual" 16-bit registers using two 8-bit registers
    | DE
    | HL


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
            -- TODO: Mask away right 4 bits as they are never written
            { registerState | f = byte }


setSixteenbitRegister :
    SixteenBitRegisterName
    -> ( Byte, Byte )
    -> RegisterState
    -> RegisterState
setSixteenbitRegister sixteenBitRegisterName ( byte1, byte2 ) registerState =
    case sixteenBitRegisterName of
        SP ->
            { registerState | sp = ( byte1, byte2 ) }

        PC ->
            { registerState | pc = ( byte1, byte2 ) }

        BC ->
            { registerState | b = byte1, c = byte2 }

        DE ->
            { registerState | d = byte1, e = byte2 }

        HL ->
            { registerState | h = byte1, l = byte2 }


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


type Instruction
    = NOP
    | STOP
      -- FIXME: We need to handle 8-bit and 16-bit loads
    | LDValue RegisterName Byte
    | LDRegister RegisterName RegisterName
    | LDD RegisterName RegisterName
    | PUSH RegisterName
    | POP RegisterName
    | ADD RegisterName
    | ADC RegisterName
    | SUB RegisterName
    | SBC RegisterName
    | AND RegisterName
    | OR RegisterName
    | XOR RegisterName
    | CP RegisterName
    | INC RegisterName
    | DEC RegisterName
    | ADDSP Byte
    | SWAP RegisterName
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
    | RLC RegisterName
    | RL RegisterName
    | RRC RegisterName
    | RR RegisterName
    | SLA RegisterName
    | SRA RegisterName
    | SRL RegisterName
    | BIT BitIndex RegisterName
    | SET BitIndex RegisterName
    | RES BitIndex RegisterName
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



-- TODO: Implement fetch


type alias EmulatorState =
    { mmu : MMU, registers : RegisterState }


fetch : EmulatorState -> ( Binary.Word, EmulatorState )
fetch emulatorState =
    ( Binary.Word zeroByte zeroByte, emulatorState )



-- TODO: Implement decode


decode : Binary.Word -> Instruction
decode word =
    NOP



-- TODO: Implement execute


execute : ( Instruction, EmulatorState ) -> EmulatorState
execute ( instruction, emulatorState ) =
    emulatorState


processInstructionCycle : EmulatorState -> EmulatorState
processInstructionCycle emulatorState =
    emulatorState
        |> fetch
        |> Tuple.mapFirst decode
        |> execute
