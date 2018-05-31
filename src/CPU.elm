module CPU exposing (..)

import Binary exposing (Bit(..), Byte, zeroByte)


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


type Instruction
    = NOP
    | STOP
    | JR Flag ( Byte, Byte )


execute : Instruction -> RegisterState -> RegisterState
execute instruction registerState =
    initialRegisterState
