module MMU exposing (..)

import Binary exposing (Word, Byte, zeroByte)
import Array exposing (Array)


type alias Memory =
    Array Word


type alias MMU =
    { rom : { banks : Array Memory, selectedBank : Int }
    , vram : Memory
    , wram : Memory
    , zram : Memory
    }


readByte : MMU -> Word -> Byte
readByte mmu address =
    zeroByte


readWord : MMU -> Word -> Word
readWord mmu address =
    Binary.Word zeroByte zeroByte
