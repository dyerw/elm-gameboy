module Emulator exposing (..)

import CPU exposing (..)
import Binary as B exposing (Bit(..))
import Decode as D
import MMU exposing (..)


-- TODO: Implement fetch


type alias EmulatorState =
    { mmu : MMU, registers : RegisterState }


fetch : EmulatorState -> ( D.OpCode, EmulatorState )
fetch emulatorState =
    ( (D.OpCode (B.Byte O O O O O O O O) Nothing Nothing), emulatorState )


type ExecuteError
    = ExecuteError String


type EmulatorError
    = EmulatorExecuteError ExecuteError
    | EmulatorDecodeError D.DecodeError


execute : ( Instruction, EmulatorState ) -> Result EmulatorError EmulatorState
execute ( instruction, emulatorState ) =
    Ok emulatorState


processInstructionCycle : EmulatorState -> Result EmulatorError EmulatorState
processInstructionCycle emulatorState =
    emulatorState
        |> fetch
        |> Tuple.mapFirst D.decode
        |> (\( instructionResult, state ) ->
                Result.map (\x -> ( x, state )) instructionResult
           )
        |> Result.mapError (\decodeError -> EmulatorDecodeError decodeError)
        |> Result.andThen execute
