module Binary exposing (Bit(..), BitIndex(..), Byte, zeroByte, Word)


type Bit
    = O -- Zero
    | I -- One


type alias Byte =
    ( Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit )


type alias Word =
    ( Byte, Byte )


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


toList : Byte -> List Bit
toList ( bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 ) =
    [ bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 ]


toTuple : List Bit -> Byte
toTuple bitList =
    case bitList of
        [ bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 ] ->
            ( bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8 )

        _ ->
            zeroByte


bitAnd : Bit -> Bit -> Bit
bitAnd bit bit2 =
    case bit of
        O ->
            case bit2 of
                O ->
                    O

                I ->
                    I

        I ->
            case bit2 of
                O ->
                    O

                I ->
                    I


bitOr : Bit -> Bit -> Bit
bitOr bit bit2 =
    case bit of
        O ->
            case bit2 of
                O ->
                    O

                I ->
                    I

        I ->
            case bit2 of
                O ->
                    I

                I ->
                    I


byteAnd : Byte -> Byte -> Byte
byteAnd byte byte2 =
    toTuple <| List.map2 bitAnd (toList byte) (toList byte2)


byteOr : Byte -> Byte -> Byte
byteOr byte byte2 =
    toTuple <| List.map2 bitOr (toList byte) (toList byte2)


bitMask : BitIndex -> Byte -> Byte
bitMask bitIndex byte =
    byteAnd (mask bitIndex) byte


mask : BitIndex -> Byte
mask bitIndex =
    case bitIndex of
        Zero ->
            ( O, O, O, O, O, O, O, O )

        One ->
            ( O, O, O, O, O, O, O, I )

        Two ->
            ( O, O, O, O, O, O, I, I )

        Three ->
            ( O, O, O, O, O, I, I, I )

        Four ->
            ( O, O, O, O, I, I, I, I )

        Five ->
            ( O, O, O, I, I, I, I, I )

        Six ->
            ( O, O, I, I, I, I, I, I )

        Seven ->
            ( O, I, I, I, I, I, I, I )
