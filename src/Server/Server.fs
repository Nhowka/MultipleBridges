open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn
open Shared
open Elmish
open Elmish.Bridge

open Giraffe.Serialization
open Microsoft.WindowsAzure.Storage
open System
open Saturn.CSRF.View

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x
let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse
let port = 8085us

module Receiver =
    let hub =
        ServerHub<_,_,string>()
    let init _ () =
        Set.empty, Cmd.none

    let update _ msg model =
        model |>
        match msg with
        | AddFilter f -> Set.add (f.ToLowerInvariant())
        | RemoveFilter f -> Set.remove (f.ToLowerInvariant())
        , Cmd.none

    let bridge =
        Bridge.mkServer Endpoint.Receiver init update
        |> Bridge.withServerHub hub
        |> Bridge.run Giraffe.server

module Broadcaster =

    let filter (msg:string) (s:Set<string>) =
        s |> Set.isEmpty
        ||
        s |> Set.exists (msg.ToLowerInvariant().Contains)

    let init _ () =
        (), Cmd.none

    let update _ (Broadcaster.Msg msg) () =
        Receiver.hub.SendClientIf(filter msg) msg
        (), Cmd.none

    let bridge =
        Bridge.mkServer Endpoint.Broadcaster init update
        |> Bridge.run Giraffe.server


let socket =
  choose [
    Receiver.bridge
    Broadcaster.bridge
  ]

let configureSerialization (services:IServiceCollection) =
    let fableJsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer fableJsonSettings)

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router socket
    memory_cache
    use_static publicPath
    service_config configureSerialization
    service_config configureAzure
    app_config Giraffe.useWebSockets
    use_gzip
}

run app
