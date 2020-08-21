namespace PolyCoder.ConsoleEx

open System

type Colors(foreground: ConsoleColor, background: ConsoleColor) =

  member val Foreground = foreground with get
  member val Background = background with get
  
  new () = Colors(ConsoleColor.Gray, ConsoleColor.Black)

  static member val WhiteOnBlack = Colors(ConsoleColor.Gray, ConsoleColor.Black)
  static member val BlackOnWhite = Colors(ConsoleColor.Black, ConsoleColor.Gray)

type ConsoleState
  (
    foreground: ConsoleColor,
    background: ConsoleColor,
    top: int,
    left: int,
    cursorVisible: bool
  ) =

  member val Foreground = foreground with get, set
  member val Background = background with get, set
  member val Top = top with get, set
  member val Left = left with get, set
  member val CursorVisible = cursorVisible with get, set

