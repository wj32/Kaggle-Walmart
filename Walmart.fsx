#load "packages/FsLab/FsLab.fsx"

fsi.ShowDeclarationValues <- false

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

let writeLine (file : System.IO.StreamWriter) cells = file.WriteLine(String.concat "," cells)

// Convert Walmart data

type WalmartCsv = CsvProvider<"G:/box/walmart/train.csv">
let data = WalmartCsv.Load("G:/box/walmart/train.csv")
let outputFileName = "G:/box/walmart/train_processed.csv"
let mapOutputFileName = "G:/box/walmart/train_processed_map.csv"

// Department as category
//type Category = string
//let categoryOfRow (row : WalmartCsv.Row) =
//  if row.DepartmentDescription <> "NULL" then
//    Option.Some row.DepartmentDescription
//  else
//    Option.None
//let categoryToString category = category

// Fineline number as category
//type Category = int
//let categoryOfRow (row : WalmartCsv.Row) = Option.ofNullable row.FinelineNumber
//let categoryToString category = "F" + (string category)

// Selective fineline number
type Category = string
let importantDepartments =
  Set.ofList
    [ "FINANCIAL SERVICES"; "IMPULSE MERCHANDISE"; "SERVICE DELI"; "LIQUOR,WINE,BEER" ]
let categoryOfRow (row : WalmartCsv.Row) =
  if Set.contains row.DepartmentDescription importantDepartments then
    if row.FinelineNumber.HasValue then
      Option.Some (row.DepartmentDescription + " " + string row.FinelineNumber.Value)
    else
      Option.None
  else
    Option.Some row.DepartmentDescription
let categoryToString category = category

module AggregatedVisit =
  type T =
    { visitNumber : int;
      tripType : int;
      weekday : string;
      purchases : Map<Category, int>;
      returns : Map<Category, int>;
      purchaseDepartments : Set<string>;
      returnDepartments : Set<string>;
      purchaseUpcs : Set<int64>; }

  let init visitNumber tripType weekday =
    { visitNumber = visitNumber;
      tripType = tripType;
      weekday = weekday;
      purchases = Map.empty;
      returns = Map.empty;
      purchaseDepartments = Set.empty;
      returnDepartments = Set.empty;
      purchaseUpcs = Set.empty; }

let categories =
  data.Rows
  |> Seq.choose categoryOfRow
  |> Seq.distinct
  |> Seq.toList
  |> List.sort

let dataByVisitList =
  let visitOfRow (row : WalmartCsv.Row) =
    AggregatedVisit.init row.VisitNumber row.TripType row.Weekday
  let visits = 
    data.Rows
    |> Seq.fold (fun (visit : AggregatedVisit.T, visits) row ->
      let (visit, visits) =
        if row.VisitNumber <> visit.visitNumber then
          (visitOfRow row, visit :: visits)
        else
          (visit, visits)
      match categoryOfRow row with
      | Some category ->
        let updateScanMap sign map =
          if row.ScanCount * sign > 0 then
            Map.add
              category
              (defaultArg (Map.tryFind category map) 0 + row.ScanCount * sign)
              map
          else
            map
        let updateDepartmentSet sign set =
          if row.ScanCount * sign > 0 then
            if row.DepartmentDescription <> "NULL" then
              Set.add row.DepartmentDescription set
            else
              set
          else
            set
        let purchaseUpcs =
          if row.ScanCount > 0 && row.Upc.HasValue then
            Set.add row.Upc.Value visit.purchaseUpcs
          else
            visit.purchaseUpcs
        let visit =
          { visit with
              purchases = updateScanMap 1 visit.purchases;
              returns = updateScanMap -1 visit.returns;
              purchaseDepartments = updateDepartmentSet 1 visit.purchaseDepartments;
              returnDepartments = updateDepartmentSet -1 visit.returnDepartments;
              purchaseUpcs = purchaseUpcs;
          }
        (visit, visits)
      | None ->
        (visit, visits)
    ) (visitOfRow (Seq.head data.Rows), [])
    |> List.Cons
  visits |> List.rev

let weekdays = ["Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"]
let header = 
  [ "TripType";
    "Weekday";
    "Purchases";
    "Returns";
    "PurchaseDepartments";
    "ReturnDepartments";
    "PurchasesPerDepartment";
    "ReturnsPerDepartment";
    "PurchaseUpcs";
    "PurchaseUpcsPerDepartment"; ]
  @ weekdays
  @ (List.map (fun category -> "\"" + categoryToString category + "\"") categories)
  @ (List.map (fun category -> "RETURN \"" + categoryToString category + "\"") categories)

using (System.IO.File.CreateText(outputFileName)) (fun outputFile ->
  let writeLine = writeLine outputFile
  writeLine header
  List.iter (fun (visit : AggregatedVisit.T) ->
    let purchaseScanCount = visit.purchases |> Map.toSeq |> Seq.map snd |> Seq.fold (+) 0
    let returnScanCount = visit.returns |> Map.toSeq |> Seq.map snd |> Seq.fold (+) 0
    let purchasesPerDepartment =
      if visit.purchaseDepartments.Count <> 0 then
        float purchaseScanCount / float visit.purchaseDepartments.Count
      else
        -1.
    let returnsPerDepartment =
      if visit.returnDepartments.Count <> 0 then
        float returnScanCount / float visit.returnDepartments.Count
      else
        -1.
    let purchaseUpcsPerDepartment =
      if visit.purchaseDepartments.Count <> 0 then
        float visit.purchaseUpcs.Count / float visit.purchaseDepartments.Count
      else
        -1.
    let row =
      [ visit.tripType |> string;
        visit.weekday;
        purchaseScanCount |> string;
        returnScanCount |> string;
        visit.purchaseDepartments.Count |> string;
        visit.returnDepartments.Count |> string;
        purchasesPerDepartment |> string;
        returnsPerDepartment |> string;
        visit.purchaseUpcs.Count |> string;
        purchaseUpcsPerDepartment |> string; ]
      @ (List.map (fun weekday -> if visit.weekday = weekday then "1" else "0") weekdays)
      @ (List.map (fun category -> defaultArg (Map.tryFind category visit.purchases) 0 |> string) categories)
      @ (List.map (fun category -> defaultArg (Map.tryFind category visit.returns) 0 |> string) categories)
    writeLine row
  ) dataByVisitList)

using (System.IO.File.CreateText(mapOutputFileName)) (fun mapOutputFile ->
  let writeLine = writeLine mapOutputFile
  writeLine ["VisitNumber"]
  List.iter (fun (visit : AggregatedVisit.T) ->
    writeLine [visit.visitNumber |> string]
  ) dataByVisitList)

// Combine test processing map and prediction files
// THIS IS WRONG, NEED TO SUBMIT PROBABILITIES NOT 1s AND 0s
//type MapCsv = CsvProvider<"G:/box/walmart/test_processed_map.csv">
//type PredictionCsv = CsvProvider<"G:/box/walmart/test_prediction.csv">
//let combinePredictions () =
//  let mapData = MapCsv.Load("G:/box/walmart/test_processed_map.csv")
//  let predictionData = PredictionCsv.Load("G:/box/walmart/test_prediction.csv")
//  let combinedOutputFileName = "G:/box/walmart/test_prediction_combined.csv"
//  let tripTypes =
//    predictionData.Rows
//    |> Seq.map (fun r -> r.TripType)
//    |> Seq.distinct
//    |> Seq.toList
//    |> List.sort
//  let combinedHeader = ["\"VisitNumber\""] @ (List.map (fun t -> "\"TripType_" + string t + "\"") tripTypes)
//  using (System.IO.File.CreateText(combinedOutputFileName)) (fun combinedOutputFile ->
//    let writeLine = writeLine combinedOutputFile
//    writeLine combinedHeader
//    Seq.iter2 (fun (mapRow : MapCsv.Row) (predictionRow : PredictionCsv.Row) ->
//      let row =
//        [mapRow.VisitNumber |> string]
//        @ (List.map (fun tripType -> if predictionRow.TripType = tripType then "1" else "0") tripTypes)
//      writeLine row
//    ) mapData.Rows predictionData.Rows)
