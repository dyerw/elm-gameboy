module Binary exposing (Bit(..), Byte, zeroByte)


type Bit
    = O -- Zero
    | I -- One


type alias Byte =
    ( Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit )


zeroByte : Byte
zeroByte =
    ( O, O, O, O, O, O, O, O )
