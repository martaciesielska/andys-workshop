// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO

[<Literal>]
let reconciliationSummaryFile = """C:\Users\Marta\Desktop\fsharpfiles\reconciliation_summary_inconsistent.csv"""

[<Literal>]
let tradesFile = """C:\Users\Marta\Desktop\fsharpfiles\trades.txt"""

type ProductSummaryInfo = {
        Name : string
        TotalQuantity : int
        TotalCash : int
    }

let getTotalsFromCsv fileName =
    let splitRecord (recordString : string) = 
        let splitStrings = recordString.Split ','
        {
            Name = splitStrings.[0]
            TotalQuantity = System.Int32.Parse splitStrings.[1]
            TotalCash = System.Int32.Parse splitStrings.[2]
        }

    File.ReadAllLines fileName
        |> Seq.skip 1
        |> Seq.map splitRecord

type TradeRecordInfo = {
    ProductId : string
    TradeType : string
    Quantity : int
    Amount : int
}

let (|ProductIdLine|_|) (line : string) =
    if line.StartsWith "ProductId: "
        then Some(line.Substring 11)
        else None

let (|TradeTypeLine|_|) (line : string) =
    if line.StartsWith "TradeType: "
        then Some(line.Substring 11)
        else None

let (|QuantityLine|_|) (line : string) =
    if line.StartsWith "Quantity: "
        then Some(System.Int32.Parse (line.Substring 10))
        else None

let (|AmountLine|_|) (line : string) =
    if line.StartsWith "Amount: "
        then Some(System.Int32.Parse (line.Substring 8))
        else None

let (|BoundaryLine|_|) (line : string) =
    if line.StartsWith "----"
        then Some BoundaryLine
        else None

let getTradesFromFile fileName =
    let defaultRecord = {
        ProductId = ""
        TradeType = ""
        Quantity = 0
        Amount = 0
    }
    let rec processLines currentRecord recordInfos lines =
        match lines with
        | [] -> recordInfos
        | ProductIdLine productId :: tail -> 
            let newRecord = {
                currentRecord with ProductId = productId
            }
            processLines newRecord recordInfos tail
        | TradeTypeLine tradeType :: tail -> 
            let newRecord = {
                currentRecord with TradeType = tradeType
            }
            processLines newRecord recordInfos tail
        | QuantityLine quantity :: tail -> 
            let newRecord = {
                currentRecord with Quantity = quantity
            }
            processLines newRecord recordInfos tail
        | AmountLine amount :: tail -> 
            let newRecord = {
                currentRecord with Amount = amount
            }
            processLines newRecord recordInfos tail
        | BoundaryLine :: tail -> 
            let newRecordInfos = currentRecord :: recordInfos
            processLines defaultRecord newRecordInfos tail
        | _ :: tail -> 
            processLines currentRecord recordInfos tail
         
    File.ReadAllLines fileName
        |> List.ofArray
        |> processLines defaultRecord []

type TradeData = {
    ProductId : string
    Quantity : int
    Cash : int
}

let (|CashTrade|_|) ri =
    if ri.TradeType = "Cash" 
        then Some(CashTrade)
        else None

let (|OptionTrade|_|) ri =
    if ri.TradeType = "Option" 
        then Some(OptionTrade)
        else None

let mapRecordInfo recordInfo = 
    match recordInfo with
    | CashTrade ->
        {
            ProductId = recordInfo.ProductId
            Quantity = 0
            Cash = recordInfo.Quantity
        }
    | OptionTrade ->
        {
            ProductId = recordInfo.ProductId
            Quantity = recordInfo.Amount
            Cash = 0
        }
    | _ ->
        {
            ProductId = recordInfo.ProductId
            Quantity = recordInfo.Quantity
            Cash = 0
        }

let aggregateTrades tradeList = 
    tradeList 
        |> List.groupBy (fun tr -> tr.ProductId) 
        |> List.map (fun (id, list) -> 
                                    { 
                                        Name = id
                                        TotalQuantity = List.sumBy (fun tr -> tr.Quantity) list
                                        TotalCash = List.sumBy (fun tr -> tr.Cash) list
                                    })

[<EntryPoint>]
let main argv = 
    let tradeSummaryList = 
        getTradesFromFile tradesFile
        |> List.map mapRecordInfo
        |> aggregateTrades
        |> List.map (fun trd -> trd.Name, trd)
        |> Map.ofList

    let productSummaryList = getTotalsFromCsv reconciliationSummaryFile |> Seq.map (fun trd -> trd.Name, trd) |> Map.ofSeq

    let reconResult = if productSummaryList = tradeSummaryList then "OK!\n"
                                                               else "Oh oh...\n"

    printfn "%s" reconResult
      
    printfn "Missing products (from summary):" 
    let missingProductSummaries =
        tradeSummaryList 
            |> Map.filter (fun key tr -> not (productSummaryList.ContainsKey key))
            |> Map.iter (fun key tr -> printfn "%s" key)

    printfn "Missing products (from trades):" 
    let missingTradeSummaries =
        productSummaryList 
            |> Map.filter (fun key tr -> not (tradeSummaryList.ContainsKey key))
            |> Map.iter (fun key tr -> printfn "%s" key)

    printfn "Discrepancies:" 
    let missingTradeSummaries =
        productSummaryList 
            |> Map.filter (fun key tr -> tradeSummaryList.ContainsKey key)
            |> Map.filter (fun key tr -> tr <> tradeSummaryList.[key])
            |> Map.iter (fun key tr -> printfn "%s %A" key tr)

    System.Console.ReadKey |> ignore
    0 // return an integer exit code
