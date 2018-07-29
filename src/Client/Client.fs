module Client

open Elmish
open Elmish.Bridge
open Elmish.React
open Fable.Core
open Fable.Helpers.React
open Fable.Core.JsInterop
open Fable.Helpers.React.Props
importAll "../../node_modules/bulma/bulma.sass"
type Content =
    {
        Filters : string list
        Messages: string list
        FilterBox: string
    }

type Model =
    { Left : Content
      Right : Content
      TextBox: string }

type MsgFor =
    | Left
    | Right
    | TextBoxSet of string

type ClientMsg =
    | NoOp
    | NewFilter
    | Reset
    | DeleteFilter of string
    | NewMessage of string
    | SetFilter of string
[<PassGenericsAttribute>]
let leftSender msg = Bridge.NamedSend("Left", msg)
[<PassGenericsAttribute>]

let rightSender msg = Bridge.NamedSend("Right", msg)

open Shared

open Fulma

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let init () =
    let content =
        {
            Filters = []
            Messages = []
            FilterBox = ""
        }
    {
        Left = content
        Right = content
        TextBox = ""
    }, Cmd.none

let contentUpdate sender msg model =
    match msg with
    | NoOp -> model
    | Reset -> { model with Filters = [] }
    | NewMessage m ->
        { model with Messages= m :: model.Messages }
    | SetFilter f ->
        { model with FilterBox = f }
    | NewFilter ->
        if model.FilterBox = "" then
            model
        else
            sender (AddFilter model.FilterBox)
            { model with
                Filters = model.FilterBox::model.Filters |> List.distinctBy (fun f -> f.ToLowerInvariant())
                FilterBox = "" }
    | DeleteFilter f ->
        sender (RemoveFilter f)
        { model with Filters = model.Filters |> List.filter((<>) f) }


let update (dsc,msg)  model =
    match dsc, model with
    | Left, {Left = left} -> { model with Left = contentUpdate leftSender msg left }
    | Right,{Right = right} -> { model with Right = contentUpdate rightSender msg right }
    | TextBoxSet text, model -> {model with TextBox = text}
    , Cmd.none

type Tokenizer =
    | Match of string
    | NoMatch of string

let normalize l =
    List.foldBack
        (fun x s ->
            match x,s with
            | Match x, (Match h)::t -> Match(x+h)::t
            | _ -> x::s) l []

let tokenSingle (needle:string) (haystack:string) =
    let n = needle.ToLowerInvariant()
    let offset = n.Length-1
    let h = haystack.ToLowerInvariant()
    let rec find (start:int) =
        [
            match h.IndexOf(n,start) with
            | -1 -> yield haystack.[start..] |> NoMatch
            | i ->
                if i <> start then
                    yield NoMatch(haystack.[start..i-1])
                let nextStart = i + offset
                yield Match(haystack.[i..nextStart])
                yield! find (nextStart+1)
        ]
    find 0 |> normalize

let tokenMultiple needles haystack =
    needles |> List.fold(fun s n -> s |> List.collect (function NoMatch h -> tokenSingle n h | x -> [x])) [NoMatch haystack] |> normalize

let print tokens =
    tokens
    |> List.map (function
        | NoMatch x -> str x
        | Match x -> span [Style [CSSProp.Color "Red"]] [str x])
    |> Container.container [Container.Props [Style[CSSProp.MaxWidth "100%"]]]


let props size : IHTMLProp list = [ Style [CSSProp.Height size;CSSProp.MaxHeight size;CSSProp.OverflowY "auto";CSSProp.OverflowX "none";CSSProp.MaxWidth "100%"]]
let contentView content dispatch =

    [
        content.Messages
        |> List.map(fun m -> tokenMultiple content.Filters m |> print)
        |> Container.container [Container.Props (props "90%")]
        Media.media[
              Media.Props [Style [CSSProp.MaxWidth "100%"]]
          ][
            Media.content[][
                Input.text [
                    Input.Placeholder "Filter"
                    Input.ValueOrDefault content.FilterBox
                    Input.OnChange (fun e -> dispatch (SetFilter (!!e.target?value)))
                    Input.Props [
                      OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
                                if ev.keyCode = Fable.PowerPack.Keyboard.Codes.enter then
                                  ev.preventDefault()
                                  dispatch NewFilter )
                    ]
                ]
               ]
            Media.right[][
                  Button.a [Button.OnClick (fun _ -> dispatch NewFilter)][str "Add filter"]
                 ]
          ]
        content.Filters
        |> List.map (fun f -> Tag.tag [Tag.Color IsDark][str f; Delete.delete[ Delete.Size IsSmall; Delete.OnClick (fun _ -> dispatch (DeleteFilter f))][]])
        |> Container.container [Container.Props [Style [CSSProp.Height "0";CSSProp.MaxHeight "80%";CSSProp.MaxWidth "100%";CSSProp.Position "relative";CSSProp.Bottom "0"]]]
    ]


let view ({Left=left;Right=right;TextBox=tx} as model) dispatch =
  let sendMsg() =
    if tx <> "" then
        Bridge.Send(Broadcaster.Msg tx)
    dispatch (TextBoxSet "",NoOp)
  div [ Style[CSSProp.Overflow "none"]]  [
    Container.container [Container.Props [Style [CSSProp.Height "100vh";CSSProp.MaxHeight "100vh";CSSProp.Overflow "none"]]] [
        Columns.columns [Columns.IsMobile] [
          Column.column [Column.Props (props "80vh")]
            (contentView left (fun msg -> dispatch (Left,msg)))
          Column.column [Column.Props (props "80vh")]
            (contentView right (fun msg -> dispatch (Right,msg)))]
        Container.container [Container.Props [Style [CSSProp.Height "10vh";CSSProp.MaxHeight "20vh";CSSProp.Position "absolute";CSSProp.Bottom "0"]]]
            [
              Media.media[][
               Media.content[][
                Input.text [
                    Input.Placeholder "Message"
                    Input.ValueOrDefault tx
                    Input.OnChange (fun e -> dispatch (TextBoxSet (!!e.target?value),NoOp))
                    Input.Props [
                      OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
                                if ev.keyCode = Fable.PowerPack.Keyboard.Codes.enter then
                                  ev.preventDefault()
                                  sendMsg())
                    ]
                ]
               ]
               Media.right[][
                  Button.a [Button.OnClick (fun _ -> sendMsg())][str "Send"]
                 ]
             ]
            ]
         ]
        ]

Program.mkProgram init update view
|> Program.withBridgeConfig (
    Bridge.endpoint Endpoint.Receiver
    |> Bridge.withName "Left"
    |> Bridge.withWhenDown (Left,Reset)
    |> Bridge.withMapping (fun msg -> (Left, NewMessage msg)))
|> Program.withBridgeConfig (
    Bridge.endpoint Endpoint.Receiver
    |> Bridge.withName "Right"
    |> Bridge.withWhenDown (Right,Reset)
    |> Bridge.withMapping (fun msg -> (Right, NewMessage msg)))
|> Program.withBridge Endpoint.Broadcaster
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
