module Emulator exposing (..)

import CPU as C
import Binary as B exposing (Bit(..))
import Decode as D
import MMU exposing (..)


-- TODO: Implement fetch


type alias EmulatorState =
    { mmu : MMU, registers : C.RegisterState }


fetch : EmulatorState -> ( D.OpCode, EmulatorState )
fetch emulatorState =
    ( (D.OpCode (B.Byte O O O O O O O O) Nothing Nothing), emulatorState )


type EmulatorError
    = EmulatorCPUError C.CPUError
    | EmulatorDecodeError D.DecodeError


processInstructionCycle : EmulatorState -> Result EmulatorError EmulatorState
processInstructionCycle emulatorState =
    emulatorState
        |> fetch
        |> Tuple.mapFirst D.decode
        |> (\( instructionResult, state ) ->
                Result.map (\x -> ( x, state )) instructionResult
           )
        |> Result.mapError (\decodeError -> EmulatorDecodeError decodeError)
        |> Result.andThen
            (\( instruction, state ) ->
                (C.execute instruction state.mmu state.registers)
                    |> Result.mapError
                        (\cpuError -> EmulatorCPUError cpuError)
                    |> Result.map (\( mmu, registers ) -> { mmu = mmu, registers = registers })
            )
