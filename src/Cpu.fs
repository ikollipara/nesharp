///<summary>
///The Cpu implementation for Nesharp.
///Contains the datastructure, module, and opcodes for running the Cpu.
///</summary>
module Nesharp.Cpu


type T =
    { RegisterA: uint8
      RegisterX: uint8
      RegisterY: uint8
      Status: uint8
      RegisterStack: uint8
      ProgramCounter: uint16
      Memory: uint8 array }


let private ReadMemoryU8 (cpu: T) (addr: uint16) : uint8 = cpu.Memory[(int addr)]
let private WriteMemoryU8 (cpu: T) (addr: uint16) (value: uint8) = cpu.Memory[int addr] <- value

let private ReadMemoryU16 (cpu: T) (addr: uint16) : uint16 =
    let lo = ReadMemoryU8 cpu addr
    let hi = ReadMemoryU8 cpu (addr + uint16 1)
    ((hi <<< 8) ||| lo) |> uint16

let private WriteMemoryU16 (cpu: T) (addr: uint16) (value: uint16) =
    let hi = (value >>> 8) |> uint8
    let lo = (value &&& uint16 0xFF) |> uint8
    WriteMemoryU8 cpu addr lo
    WriteMemoryU8 cpu (addr + uint16 1) hi

exception InvalidAddressingMode of string

[<RequireQualifiedAccess>]
type AddressingMode =
    | Immediate
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | IndirectX
    | IndirectY
    | None


    member this.GetOperand(cpu: T) : uint16 =
        match this with
        | AddressingMode.Immediate -> cpu.ProgramCounter
        | AddressingMode.ZeroPage -> ReadMemoryU8 cpu cpu.ProgramCounter |> uint16
        | AddressingMode.Absolute -> ReadMemoryU16 cpu cpu.ProgramCounter
        | AddressingMode.ZeroPageX -> ReadMemoryU8 cpu cpu.ProgramCounter |> (+) cpu.RegisterX |> uint16
        | AddressingMode.ZeroPageY -> ReadMemoryU8 cpu cpu.ProgramCounter |> (+) cpu.RegisterY |> uint16
        | AddressingMode.AbsoluteX -> ReadMemoryU16 cpu cpu.ProgramCounter |> (+) (uint16 cpu.RegisterX)
        | AddressingMode.AbsoluteY -> ReadMemoryU16 cpu cpu.ProgramCounter |> (+) (uint16 cpu.RegisterY)
        | AddressingMode.IndirectX ->
            let ptr = ReadMemoryU8 cpu cpu.ProgramCounter |> uint8 |> (+) cpu.RegisterX
            let lo = ReadMemoryU8 cpu (uint16 ptr)
            let hi = ReadMemoryU8 cpu (ptr + uint8 1 |> uint16)
            (uint16 hi) <<< 8 ||| (uint16 lo)
        | AddressingMode.IndirectY ->
            let base_ = ReadMemoryU8 cpu cpu.ProgramCounter
            let lo = ReadMemoryU16 cpu (uint16 base_)
            let hi = ReadMemoryU16 cpu (base_ + uint8 1 |> uint16)
            let deref_base = (uint16 hi) <<< 8 ||| (uint16 lo)
            let deref = deref_base + (uint16 cpu.RegisterY)
            deref
        | AddressingMode.None -> InvalidAddressingMode("Invalid Addressing Mode") |> raise





let Reset (cpu: T) =
    { cpu with
        RegisterA = uint8 0
        RegisterX = uint8 0
        Status = uint8 0
        ProgramCounter = ReadMemoryU16 cpu (uint16 0xFFC) }

let Load (cpu: T) (program: uint8 array) =
    cpu.Memory[0x8000 .. (0x8000 + Array.length program)] <- program
    WriteMemoryU16 cpu (uint16 0xFFC) (uint16 0x8000)

let Create () : T =
    { RegisterA = uint8 0
      RegisterStack = uint8 0
      RegisterX = uint8 0
      RegisterY = uint8 0
      Status = uint8 0
      Memory = Array.zeroCreate 0xFFFF
      ProgramCounter = uint16 0 }
