open System
open System.Net
open System.Net.NetworkInformation
open System.Net.Sockets

/// Network adapter information
type AdapterInfo = {
    Name: string;
    DisplayAddress: string;
    PrefixLength: int;
    AddressValue: int
}

let EXIT_NORMAL = 0
let EXIT_INCORRECT_PARAM = 1

let PING_TIMEOUT = 2000

/// Convert big-endian IP address values array to integer
let bigEndianIp4ByteArrayToInt (ip4Bytes : byte[]) : int =
    (int ip4Bytes.[0] <<< 24) |||
    (int ip4Bytes.[1] <<< 16) |||
    (int ip4Bytes.[2] <<< 8) |||
    (int 1)

/// Convert IP address value to big-endian array
let intIp4ToBigEndianArray (ip4Value : int) : byte[] =
    [|
        byte ((ip4Value >>> 24) &&& 0xff);
        byte ((ip4Value >>> 16) &&& 0xff);
        byte ((ip4Value >>> 8) &&& 0xff);
        byte (ip4Value &&& 0xff)
    |]

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
        let addrBytes = ip4Addr.Address.GetAddressBytes()
                               
        {
            Name = ni.Name;
            DisplayAddress = ip4Addr.Address.ToString();
            PrefixLength = ip4Addr.PrefixLength;
            AddressValue = bigEndianIp4ByteArrayToInt addrBytes;
        })

/// Display list of available adapters to scan
let displayAdapters (adapters: AdapterInfo[]) =
    Array.iteri<AdapterInfo> 
                (fun i info -> printfn "%d. %s (%s/%d)" (i + 1) 
                                                        info.Name 
                                                        info.DisplayAddress 
                                                        info.PrefixLength)
                adapters

/// Get number of hosts in network with given info
let getNumberOfHosts (adapter: AdapterInfo) : int =
    1 <<< (32 - adapter.PrefixLength) - 1

/// Ping adapter subnet range
let pingAdapterSubnet (adapter: AdapterInfo): seq<string> =
    Seq.init (getNumberOfHosts adapter)
             (fun n -> intIp4ToBigEndianArray (adapter.AddressValue + n))
    |> Seq.map (fun hostBytes ->
        async {
            let hostIpAddr = new IPAddress(hostBytes) in
            let pingTask = (new Ping()).SendPingAsync(hostIpAddr, PING_TIMEOUT)
            let! result = Async.AwaitTask(pingTask)
            return match result.Status with
                    | IPStatus.Success -> Some hostIpAddr
                    | _ -> None
        }) |> Async.Parallel |> Async.RunSynchronously
           |> Seq.filter (fun r -> r.IsSome)
           |> Seq.map (fun s -> s.Value.ToString())

[<EntryPoint>]
let main _ = 
    Console.WriteLine("Available network adapters:")

    let adapterInfos = getAdapters() in
    displayAdapters(adapterInfos)

    Console.Write("Select adapter to ping it's range: ")

    let num = ref 0
    if Int32.TryParse(Console.ReadLine(), num) then
        let index = !num - 1
        if index >= 0 && index < adapterInfos.Length then
            let selectedAdapter = adapterInfos.[index]
            printfn "\nSelected: %s" selectedAdapter.Name
            
            let availableIps = pingAdapterSubnet selectedAdapter in
                Seq.iter (fun ip -> Console.WriteLine (ip : string)) availableIps

            Console.WriteLine("Done!")
        else
            Console.WriteLine("Incorrect adapter number!")
    else
        Console.WriteLine("Incorrect input!")

    Console.ReadKey() |> ignore
    EXIT_NORMAL
