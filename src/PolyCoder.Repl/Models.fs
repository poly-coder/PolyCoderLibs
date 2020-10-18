namespace PolyCoder.Repl.Models

type NamesList = string * string list

type ReplDescModel = 
  {
    summary: string option
    description: string option
  }

type ReplExampleModel = 
  {
    desc: ReplDescModel
    command: string
  }

type ReplCommandModel = 
  {
    desc: ReplDescModel
    names: NamesList
    examples: ReplExampleModel list
    parameters: ReplParamModel list
  }

and ReplParamModel =
  {
    desc: ReplDescModel
    names: NamesList
    examples: ReplExampleModel list
  }

type ReplModel =
  {
    version: string
    desc: ReplDescModel
  }
