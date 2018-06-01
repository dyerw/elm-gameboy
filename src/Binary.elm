module Binary exposing (..)


type Bit
    = O -- Zero
    | I -- One


type Byte
    = Byte Bit Bit Bit Bit Bit Bit Bit Bit


type Word
    = Word Byte Byte


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
