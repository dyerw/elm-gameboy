module Binary exposing (..)

-- TODO: Break out Hex package


type Bit
    = O -- Zero
    | I -- One


type Nibble
    = Nibble Bit Bit Bit Bit


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
    = ZeroIndex
    | OneIndex
    | TwoIndex
    | ThreeIndex
    | FourIndex
    | FiveIndex
    | SixIndex
    | SevenIndex


zeroByte : Byte
zeroByte =
    Byte O O O O O O O O


fullByte : Byte
fullByte =
    Byte I I I I I I I I


binaryNibbleToHexNibble : Nibble -> HexNibble
binaryNibbleToHexNibble nibble =
    case nibble of
        Nibble O O O O ->
            H0

        Nibble O O O I ->
            H1

        Nibble O O I O ->
            H2

        Nibble O O I I ->
            H3

        Nibble O I O O ->
            H4

        Nibble O I O I ->
            H5

        Nibble O I I O ->
            H6

        Nibble O I I I ->
            H7

        Nibble I O O O ->
            H8

        Nibble I O O I ->
            H9

        Nibble I O I O ->
            HA

        Nibble I O I I ->
            HB

        Nibble I I O O ->
            HC

        Nibble I I O I ->
            HD

        Nibble I I I O ->
            HE

        Nibble I I I I ->
            HF


binaryByteToHexByte : Byte -> HexByte
binaryByteToHexByte (Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8) =
    HexByte (binaryNibbleToHexNibble (Nibble bit1 bit2 bit3 bit4)) (binaryNibbleToHexNibble (Nibble bit5 bit6 bit7 bit8))


byteToHexString : Byte -> String
byteToHexString (Byte bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8) =
    ( binaryNibbleToHexNibble (Nibble bit1 bit2 bit3 bit4), binaryNibbleToHexNibble (Nibble bit5 bit6 bit7 bit8) )
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
        ZeroIndex ->
            Byte O O O O O O O O

        OneIndex ->
            Byte O O O O O O O I

        TwoIndex ->
            Byte O O O O O O I I

        ThreeIndex ->
            Byte O O O O O I I I

        FourIndex ->
            Byte O O O O I I I I

        FiveIndex ->
            Byte O O O I I I I I

        SixIndex ->
            Byte O O I I I I I I

        SevenIndex ->
            Byte O I I I I I I I
