﻿open System
open System.Net.NetworkInformation
open System.Net.Sockets

/// Network adapter information
type AdapterInfo = {
    Name: string;
    Address: string
}

let EXIT_NORMAL = 0
let EXIT_INCORRECT_PARAM = 1

let NUM_ADDRESSES = 254
let PING_TIMEOUT = 1000

/// Get network adapters information
let getAdapters () : AdapterInfo[] =
    NetworkInterface.GetAllNetworkInterfaces()
    |> Array.filter (fun ni ->
        ni.NetworkInterfaceType <> NetworkInterfaceType.Loopback &&
        ni.OperationalStatus = OperationalStatus.Up)
    |> Array.map (fun ni -> 
        let ip4Addr = ni.GetIPProperties().UnicastAddresses
                      |> Seq.find (fun addr ->
                        let family = addr.Address.AddressFamily in
                        family = AddressFamily.InterNetwork)
                               
        {
            Name = ni.Name;
            Address = ip4Addr.Address.ToString()
        })

/// Display list of available adapters to scan
let displayAdapters (adapters: AdapterInfo[]) =
    Array.iteri<AdapterInfo> (fun i info -> 
                    printfn "%d. %s (%s)" (i + 1) info.Name info.Address)
                adapters

/// Ping adapter subnet range
let pingAdapterSubnet (adapter: AdapterInfo): unit =
    let prefix = adapter.Address.Substring(0, adapter.Address.LastIndexOf('.'))
    [1..NUM_ADDRESSES]
    |> List.map (fun hostNum ->
        async {
            let hostIpAddr = sprintf "%s.%d" prefix hostNum in
            let pingTask = (new Ping()).SendPingAsync(hostIpAddr, PING_TIMEOUT)
            let! result = Async.AwaitTask(pingTask)
            if result.Status = IPStatus.Success then
                printfn "%s - available" hostIpAddr
        }) |> Async.Parallel |> Async.RunSynchronously |> ignore

[<EntryPoint>]
let main argv = 
    Console.WriteLine("Select network adapter:")

    let adapterInfos = getAdapters() in
    displayAdapters(adapterInfos)

    let num = ref 0
    if Int32.TryParse(Console.ReadLine(), num) then
        let index = !num - 1
        if index >= 0 && index < adapterInfos.Length then
            let selectedAdapter = adapterInfos.[index]
            printfn "Selected: %s" selectedAdapter.Name
            pingAdapterSubnet selectedAdapter
            Console.WriteLine("Done!")
        else
            Console.WriteLine("Incorrect adapter number!")
    else
        Console.WriteLine("Incorrect input!")

    Console.ReadKey() |> ignore
    EXIT_NORMAL
