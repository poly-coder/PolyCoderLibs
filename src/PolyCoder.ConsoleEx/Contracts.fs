namespace PolyCoder.ConsoleEx.Contracts

// Inspired by https://github.com/goblinfactory/konsole

open System
open System.Runtime.InteropServices
open PolyCoder.ConsoleEx

type ISetColors =
  abstract member ForegroundColor: ConsoleColor with get, set
  abstract member BackgroundColor: ConsoleColor with get, set
  
  abstract member Colors: Colors with get, set

type IPrintAt =
  abstract member PrintAt: x: int * y: int * format: string * [<ParamArray>] args: obj[] -> unit
  abstract member PrintAt: x: int * y: int * text: string -> unit
  abstract member PrintAt: x: int * y: int * c: char -> unit

type IPrintAtColor =
  inherit IPrintAt
  inherit ISetColors

  abstract member PrintAtColor: foreground: ConsoleColor * x: int * y: int * text: string * ?background: ConsoleColor -> unit

type IConsoleState =
  inherit ISetColors

  abstract member State: ConsoleState with get, set
  
  abstract member CursorTop: int with get, set
  abstract member CursorLeft: int with get, set
  abstract member CursorVisible: bool with get, set

  abstract member DoCommand: console: IConsoleState * action: Action -> unit;

type IWrite =
  abstract member WriteLine: format: string * [<ParamArray>] args: obj[] -> unit
  abstract member WriteLine: text: string -> unit
  abstract member Write: format: string * [<ParamArray>] args: obj[] -> unit
  abstract member Write: text: string -> unit
  abstract member Clear: unit -> unit

type IWriteColor =
  inherit IWrite
  inherit ISetColors

  abstract member WriteLine: color: ConsoleColor * format: string * [<ParamArray>] args: obj[] -> unit
  abstract member WriteLine: color: ConsoleColor * text: string -> unit
  abstract member Write: color: ConsoleColor * format: string * [<ParamArray>] args: obj[] -> unit
  abstract member Write: color: ConsoleColor * text: string -> unit
  abstract member Clear: ?backgroundColor: ConsoleColor -> unit

type IWindowed =
  abstract member AbsoluteX: int with get
  abstract member AbsoluteY: int with get

  abstract member WindowWidth: int with get
  abstract member WindowHeight: int with get

type IScrolling =
  abstract member MoveBufferArea: sourceLeft: int * sourceTop: int * sourceWidth: int * sourceHeight: int * targetLeft: int * targetTop: int * sourceChar: char * sourceForeColor: ConsoleColor * sourceBackColor: ConsoleColor -> unit
  abstract member ScrollDown: unit -> unit

type IScrollingWindow =
  inherit IWindowed
  inherit IScrolling

type IConsole =
  inherit IPrintAtColor
  inherit IConsoleState
  inherit IWriteColor
  inherit IScrollingWindow

type IKeyboard =
  abstract member ReadKey: [<DefaultParameterValue(false)>] ?intercept: bool -> ConsoleKeyInfo
  abstract member WaitForKeyPress: [<ParamArray>] chars: char[] -> unit
  abstract member OnCharPressed: chars: char[] * key: Action<char> -> unit
  abstract member OnCharPressed: c: char * key: Action<char> -> unit
