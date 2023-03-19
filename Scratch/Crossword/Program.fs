open System.Text

type Cell = {
    IsStartCell : bool
    WordsFromHere: int
    Char : char
}

type Crossword = {
    RowCount : int
    ColumnCount : int
    Cells : Cell voption[][]
}

let MakeCell initialCount =
    let isStartCell = initialCount > 0
    {IsStartCell = isStartCell; WordsFromHere = initialCount; Char = ' '}


let MakeCrossword (boardDescription : string) =
    let charToInt c = int c - int '0'

    let makeCell c =
        match c with
        | '.'   -> ValueNone
        | count -> ValueSome (MakeCell (charToInt count))

    let rows = boardDescription.Split("\n")
    let cells = rows |> Array.map (fun row -> row |> Seq.map makeCell |> Seq.toArray) |> Seq.toArray
    let rowCount = cells.Length
    let columnCount = cells[0].Length

    {RowCount = rowCount; ColumnCount = columnCount; Cells = cells}

let AllCells crossword =
    seq {
        for row in 0 .. crossword.RowCount - 1 do
            for column in 0 .. crossword.ColumnCount - 1 do
                let cell = crossword.Cells[row][column]
                if cell.IsSome then
                    (row, column, cell.Value)
    }

let CrosswordToString crossword =
    let cellToChar = function
        | ValueSome c    -> c.Char
        | ValueNone      -> '.'

    let makeRow (builder : StringBuilder) row =
        (builder, row) ||> Array.fold(fun builder c -> builder.Append(cellToChar c)) |> ignore
        builder.AppendLine()

    let builder = (StringBuilder(), crossword.Cells) ||> Array.fold (fun builder row -> makeRow builder row)

    builder.ToString()

let IsMatch (wordFromCrossword : string) (candidate : string) =
    let rec matchStrings (left : string) (right : string) (index : int) =
        if index = left.Length then 
            true
        else
            match left[index], right[index] with
            | ' ', _            -> matchStrings left right (index + 1)
            | a, b when a = b   -> matchStrings left right (index + 1)
            | _                 -> false       

    if wordFromCrossword.Length = candidate.Length then
        matchStrings wordFromCrossword candidate 0
    else
        false

let CanGoRight row column crossword =
    if column + 1 = crossword.ColumnCount then
        false
    else
        (crossword.Cells[row][column + 1]).IsSome

let CanGoDown row column crossword =
    if row + 1 = crossword.RowCount then
        false
    else
        (crossword.Cells[row + 1][column]).IsSome

let GetWordRight row column crossword =
    let builder = StringBuilder()

    let rec loop row column crossword (builder : StringBuilder) =
        if column = crossword.ColumnCount then
            builder
        else
            match crossword.Cells[row][column] with
            | ValueSome cell-> builder.Append(cell.Char) |> ignore
                               loop row (column + 1) crossword builder
            | _             -> builder

    (loop row column crossword builder).ToString()


let GetWordDown row column crossword =
    let builder = StringBuilder()

    let rec loop row column crossword (builder : StringBuilder) =
        if row = crossword.RowCount then
            builder
        else
            match crossword.Cells[row][column] with
            | ValueSome cell-> builder.Append(cell.Char) |> ignore
                               loop (row + 1) column crossword builder
            | _             -> builder

    (loop row column crossword builder).ToString()

let SetWordRight row column crossword (word : string) delta =
    let cell = (crossword.Cells[row][column]).Value
    let newWordsFromHere = cell.WordsFromHere + delta
    crossword.Cells[row][column] <- ValueSome {cell with WordsFromHere = newWordsFromHere}

    let rec loop row column crossword (word : string) index =
        if index = word.Length then
            ()
        else
            let cell = (crossword.Cells[row][column]).Value
            crossword.Cells[row][column] <- ValueSome {cell with Char = word[index]}
            loop row (column + 1) crossword word (index + 1)

    loop row column crossword word 0

let SetWordDown row column crossword (word : string) delta =
    let cell = (crossword.Cells[row][column]).Value
    let newWordsFromHere = cell.WordsFromHere + delta
    crossword.Cells[row][column] <- ValueSome {cell with WordsFromHere = newWordsFromHere}

    let rec loop row column crossword (word : string) index =
        if index = word.Length then
            ()
        else
            let cell = (crossword.Cells[row][column]).Value
            crossword.Cells[row][column] <- ValueSome {cell with Char = word[index]}
            loop (row + 1) column crossword word (index + 1)

    loop row column crossword word 0


let rec SolveCrossword (words : Set<string>) crossword =
    let cell = crossword |> AllCells |> Seq.tryFind (fun (_, _, cell) -> cell.IsStartCell && cell.WordsFromHere > 0)

    let solveRight row column (words : Set<string>) crossword =
        let pattern = GetWordRight row column crossword
        words |> Seq.exists (fun word ->
            if IsMatch pattern word then
                let subset = words.Remove(word)
                SetWordRight row column crossword word -1

                if SolveCrossword subset crossword then
                    true
                else
                    SetWordRight row column crossword pattern 1 |> ignore
                    false
            else
                false)

    let solveDown row column (words : Set<string>) crossword =
        let pattern = GetWordDown row column crossword
        words |> Seq.exists (fun word ->
            if IsMatch pattern word then
                let subset = words.Remove(word)
                SetWordDown row column crossword word -1

                if SolveCrossword subset crossword then
                    true
                else
                    SetWordDown row column crossword pattern 1 |> ignore
                    false
            else
                false)

    match cell with
    | None  -> true
    | Some (row, column, cell) ->
        let solvedRight =   if CanGoRight row column crossword then
                                solveRight row column words crossword
                            else
                                false

        let solvedDown =    if solvedRight then
                                true
                            else    
                                if CanGoDown row column crossword then
                                    solveDown row column words crossword
                                else
                                    false

        solvedDown
        
let doWords () =
    let puzzle = "2001\n0..0\n1000\n0..0"
    let words = [|"casa"; "alan"; "ciao"; "anta"|] |> Set.ofArray
    let crossword = MakeCrossword puzzle
    SolveCrossword words crossword |> ignore
    crossword |> CrosswordToString |> printfn "%s"

let doHolidays () =
    let puzzle =   "...1...........\n\
                    ..1000001000...\n\
                    ...0....0......\n\
                    .1......0...1..\n\
                    .0....100000000\n\
                    100000..0...0..\n\
                    .0.....1001000.\n\
                    .0.1....0.0....\n\
                    .10000000.0....\n\
                    .0.0......0....\n\
                    .0.0.....100...\n\
                    ...0......0....\n\
                    ..........0...."

    let words =  [|
        "sun";
        "sunglasses";
        "suncream";
        "swimming";
        "bikini";
        "beach";
        "icecream";
        "tan";
        "deckchair";
        "sand";
        "seaside";
        "sandals" |] |> Set.ofArray

    let crossword = MakeCrossword puzzle
    SolveCrossword words crossword |> ignore
    crossword |> CrosswordToString |> printfn "%s"

let doFood () =
    let puzzle =   "..1.1..1...\n\
                    10000..1000\n\
                    ..0.0..0...\n\
                    ..1000000..\n\
                    ..0.0..0...\n\
                    1000..10000\n\
                    ..0.1..0...\n\
                    ....0..0...\n\
                    ..100000...\n\
                    ....0..0...\n\
                    ....0......"

    let words = [|
        "popcorn";
        "fruit";
        "flour";
        "chicken";
        "eggs";
        "vegetables";
        "pasta";
        "pork";
        "steak";
        "cheese"|] |> Set.ofArray

    let crossword = MakeCrossword puzzle
    SolveCrossword words crossword |> ignore
    crossword |> CrosswordToString |> printfn "%s"

doWords ()
printfn "-----------------"
doHolidays ()
printfn "-----------------"
doFood ()

printfn "Done"
