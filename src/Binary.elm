module Binary exposing (Bit(..), BitIndex(..), Byte, zeroByte)


type Bit
    = O -- Zero
    | I -- One


type alias Byte =
    ( Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit )


type BitIndex
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven


zeroByte : Byte
zeroByte =
    ( O, O, O, O, O, O, O, O )
