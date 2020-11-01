open System
open System.Net
open System.Net.NetworkInformation
open System.Net.Sockets

type Process = System.Diagnostics.Process

/// Actions from command line params
type CommandLineAction =
    | Interactive
    | Help
    | ListAdapters
    | AdapterByNumber of int
    | AdapterByIp of string
    | Incorrect

/// Network adapter information
type AdapterInfo = {
    Name: string;
    DisplayAddress: string;
    PrefixLength: int;
    AddressValue: int
}

let EXIT_NORMAL = 0
let EXIT_INCORRECT_PARAM = 1

let PING_TIMEOUT = 5000

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

/// Ping adapter's IP4 range and display results
let pingAndDisplay (adapter: AdapterInfo) =
    let availableIps = pingAdapterSubnet adapter in
    Seq.iter (fun ip -> Console.WriteLine (ip : string)) availableIps

/// Ask user for adapter and ping range
let doInteractivePing () : int =

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
            
            pingAndDisplay selectedAdapter

            Console.WriteLine("Done! Press any key to exit...")

            Console.ReadKey() |> ignore
            EXIT_NORMAL
        else
            Console.WriteLine("Incorrect adapter number!")
            EXIT_INCORRECT_PARAM
    else
        Console.WriteLine("Incorrect input!")
        EXIT_INCORRECT_PARAM

/// Show program usage and exit
let doShowHelp () : int =
    let exeName = Process.GetCurrentProcess().ProcessName in
    Console.WriteLine("Usage: {0} [(-l | -h | -a <num_or_ip>)]", exeName)

    Console.WriteLine("\nParameters:")
    Console.WriteLine("  -l\t\tList all available network adapters")
    Console.WriteLine("  -h\t\tShow this message")
    Console.WriteLine("  -a <num>\tPing range of adapter with number <num> in list")
    Console.WriteLine("  -a <ip>\tPing range of adapter with given <ip> address")

    Console.WriteLine("\nRun without parameters - interactive mode")

    EXIT_NORMAL

/// Display list of available network adapters and exit
let doListAdapters () : int =
    let adapterInfos = getAdapters() in 
    if adapterInfos.Length > 0
        then displayAdapters adapterInfos
        else Console.WriteLine("No network adapters available!")

    EXIT_NORMAL

/// Ping range of n-th adapter
let doPingByAdapterNumber (number: int) : int =
    let listIndex = number - 1
    let adapterInfos = getAdapters() in
    if listIndex >= 0 && listIndex < adapterInfos.Length
        then
            pingAndDisplay adapterInfos.[listIndex]
            EXIT_NORMAL
        else
            Console.WriteLine("Incorrect adapter number! See output with -l parameter")
            EXIT_INCORRECT_PARAM

let doPingByAdapterIp (ipPrefix: string) : int =
    let adaptersWithPrefix =
        getAdapters()
        |> Array.filter (fun ai -> ai.DisplayAddress.StartsWith(ipPrefix))

    if adaptersWithPrefix.Length > 0
        then
            pingAndDisplay adaptersWithPrefix.[0]
            EXIT_NORMAL
        else
            Console.WriteLine("Incorrect adapter IP4 address!")
            EXIT_INCORRECT_PARAM

/// Parse command line for given action
let getCommandLineAction (args: string[]) : CommandLineAction =
    if args.Length > 0
        then
            match args.[0] with
                | "-l" -> ListAdapters
                | "-h" -> Help
                | "-a" -> if args.Length = 2
                            then
                                let num = ref 0
                                if Int32.TryParse(args.[1], num)
                                    then AdapterByNumber !num
                                    else AdapterByIp args.[1]
                            else Incorrect
                | _ -> Help

        else Interactive

[<EntryPoint>]
let main (args: string[]) = 

    match getCommandLineAction args with
        | Interactive -> doInteractivePing()
        | Help -> doShowHelp()
        | ListAdapters -> doListAdapters()
        | AdapterByNumber num -> doPingByAdapterNumber num
        | AdapterByIp ip -> doPingByAdapterIp ip
        | _ ->  Console.WriteLine("Invalid parameter!")
                EXIT_INCORRECT_PARAM
