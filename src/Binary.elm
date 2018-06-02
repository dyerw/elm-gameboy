module Binary exposing (..)

-- TODO: Break out Hex package


type Bit
    = O -- Zero
    | I -- One


type Byte
    = Byte Bit Bit Bit Bit Bit Bit Bit Bit


type Word
    = Word Byte Byte


type HexNibble
    = H0
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
    | H7
    | H8
    | H9
    | HA
    | HB
    | HC
    | HD
    | HE
    | HF


type HexByte
    = HexByte HexNibble HexNibble


hexNibbleToString : HexNibble -> String
hexNibbleToString hexNibble =
    case hexNibble of
        H0 ->
            "0"

        H1 ->
            "1"

        H2 ->
            "2"

        H3 ->
            "3"

        H4 ->
            "4"

        H5 ->
            "5"

        H6 ->
            "6"

        H7 ->
            "7"

        H8 ->
            "8"

        H9 ->
            "9"

        HA ->
            "A"

        HB ->
            "B"

        HC ->
            "C"

        HD ->
            "D"

        HE ->
            "E"

        HF ->
            "F"


stringToHexNibble : String -> Maybe HexNibble
stringToHexNibble string =
    case string of
        "0" ->
            Just H0

        "1" ->
            Just H1

        "2" ->
            Just H2

        "3" ->
            Just H3

        "4" ->
            Just H4

        "5" ->
            Just H5

        "6" ->
            Just H6

        "7" ->
            Just H7

        "8" ->
            Just H8

        "9" ->
            Just H9

        "A" ->
            Just HA

        "B" ->
            Just HB

        "C" ->
            Just HC

        "D" ->
            Just HD

        "E" ->
            Just HE

        "F" ->
            Just HF

        _ ->
            Nothing


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
    Byte O O O O O O O O


fullByte : Byte
fullByte =
    Byte I I I I I I I I


binaryNibbleToHexNibble : ( Bit, Bit, Bit, Bit ) -> HexNibble
binaryNibbleToHexNibble nibble =
    case nibble of
        ( O, O, O, O ) ->
            H0

        ( O, O, O, I ) ->
            H1

        ( O, O, I, O ) ->
            H2

        ( O, O, I, I ) ->
            H3

        ( O, I, O, O ) ->
            H4

        ( O, I, O, I ) ->
            H5

        ( O, I, I, O ) ->
            H6

        ( O, I, I, I ) ->
            H7

        ( I, O, O, O ) ->
            H8

        ( I, O, O, I ) ->
            H9

        ( I, O, I, O ) ->
            HA

        ( I, O, I, I ) ->
            HB

        ( I, I, O, O ) ->
            HC

        ( I, I, O, I ) ->
            HD

        ( I, I, I, O ) ->
            HE

        ( I, I, I, I ) ->
            HF


binaryByteToHexByte : Byte -> HexByte
binaryByteToHexByte (Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8) =
    HexByte (binaryNibbleToHexNibble ( bit1, bit2, bit3, bit4 )) (binaryNibbleToHexNibble ( bit5, bit6, bit7, bit8 ))


byteToHexString : Byte -> String
byteToHexString (Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8) =
    ( binaryNibbleToHexNibble ( bit1, bit2, bit3, bit4 ), binaryNibbleToHexNibble ( bit5, bit6, bit7, bit8 ) )
        |> Tuple.mapFirst hexNibbleToString
        |> Tuple.mapSecond hexNibbleToString
        |> (\( nib1, nib2 ) -> nib1 ++ nib2)


toList : Byte -> List Bit
toList (Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8) =
    [ bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 ]


toByte : List Bit -> Byte
toByte bitList =
    case bitList of
        [ bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 ] ->
            Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8

        _ ->
            zeroByte


bitAnd : Bit -> Bit -> Bit
bitAnd bit1 bit2 =
    case ( bit1, bit2 ) of
        ( O, O ) ->
            O

        ( I, O ) ->
            O

        ( I, I ) ->
            I

        ( O, I ) ->
            O


bitOr : Bit -> Bit -> Bit
bitOr bit1 bit2 =
    case ( bit1, bit2 ) of
        ( O, O ) ->
            O

        ( O, I ) ->
            I

        ( I, O ) ->
            I

        ( I, I ) ->
            I


byteAnd : Byte -> Byte -> Byte
byteAnd byte byte2 =
    toByte <| List.map2 bitAnd (toList byte) (toList byte2)


byteOr : Byte -> Byte -> Byte
byteOr byte byte2 =
    toByte <| List.map2 bitOr (toList byte) (toList byte2)


bitMask : BitIndex -> Byte -> Byte
bitMask bitIndex byte =
    byteAnd (mask bitIndex) byte


mask : BitIndex -> Byte
mask bitIndex =
    case bitIndex of
        Zero ->
            Byte O O O O O O O O

        One ->
            Byte O O O O O O O I

        Two ->
            Byte O O O O O O I I

        Three ->
            Byte O O O O O I I I

        Four ->
            Byte O O O O I I I I

        Five ->
            Byte O O O I I I I I

        Six ->
            Byte O O I I I I I I

        Seven ->
            Byte O I I I I I I I
