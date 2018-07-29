namespace Shared

type ReceiverMsg =
    | AddFilter of string
    | RemoveFilter of string

type Broadcaster =
    | Msg of string

module Endpoint =
    let [<Literal>] Broadcaster = "/socket/broadcaster"
    let [<Literal>] Receiver = "/socket/receiver"
