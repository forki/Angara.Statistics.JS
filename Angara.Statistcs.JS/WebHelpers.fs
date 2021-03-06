﻿[<ReflectedDefinition>]
module StatisticsHelpers

type sampledFunctionType = {x: float array; f: float array}

type combinedSummaryType= {
    name:string;
    ``type``:string;    
    count:int;
    totalCount:int;
    max:float;
    min:float;
    mean:float;
    variance:float;
    lb68:float;
    ub68:float;
    lb95:float;
    ub95:float;
    median:float;
}

type correlationMatrixType = {
    r: float array array;
    c: string array;
}

type TableViewerSource = {
    summary: combinedSummaryType array;
    pdf: sampledFunctionType array;
    data: float array array;
    correlation: correlationMatrixType
}

open Statistics

let getTableViewerSource (columnNames:string array) (columnData:float array array) =
    let len = columnNames.Length
    let summaries = [| for i=0 to len-1 do yield summary columnData.[i] |]
    let qsummaries = [| for i=0 to len-1 do yield qsummary columnData.[i] |]

    let combinedSummaries =
        let merger i sum qsum =
            {
                name= columnNames.[i];
                ``type``="numeric";
                count = sum.count;
                totalCount = sum.count;
                max = sum.max;
                min = sum.min;
                mean = sum.mean;
                variance = sum.variance;
                lb68 = qsum.lb68;
                ub68 = qsum.ub68;
                lb95 = qsum.lb95;
                ub95 = qsum.ub95;
                median = qsum.median;
            }
        Array.mapi2 merger summaries qsummaries
    
    //power of 2 less then v
    let exp2ceiling v = 
        let exps = [4.0;8.0;16.0;32.0;64.0;128.0;256.0;512.0;1024.0]
        let rec check (v:float) exps =
            match exps with
            |   head::next::tail  ->
                if (next>=v) then
                    head
                else
                    check v (next::tail)
            |   head::[]    -> head
        check v exps
                

    let pdf_samples = exp2ceiling (float ((columnData.[0].Length)/2)) //power of 2 less then 
    
    let pdfs =        
        let f = Statistics.kde (int pdf_samples)
        let cast a =
            let (x,y) = a
            {x=x;f=y}

        Array.map (f >> cast) columnData

    let corrMatrixData =
        [|
            for i=0 to len-2 do
                yield [|
                        for j=i+1 to len-1 do
                            yield correlation (columnData.[i]) (columnData.[j])
                            |]
        |]

    {
        ``summary``= combinedSummaries;
        data= columnData;
        pdf=pdfs;
        correlation= {
            r= corrMatrixData;
            c=columnNames;
        };
    }
        