type Cell = {
    IsStartCell : bool
    WordsFromHere: int
    Char : char
}

type Board = {
    RowCount : int
    ColumnCount : int
    Cells : Cell option[][]
}

let MakeCell initialCount =
    let isStartCell = initialCount > 0
    {IsStartCell = isStartCell; WordsFromHere = initialCount; Char = ' '}


let MakeCrossword (boardDescription : string) =
    let charToInt c = int c - int '0'

    let makeCell c =
        match c with
        | '.'   -> None
        | count -> Some (MakeCell (charToInt count))

    let rows = boardDescription.Split("\n")
    let cells = rows |> Array.map (fun row -> row |> Seq.map makeCell |> Seq.toArray) |> Seq.toArray
    let rowCount = cells.Length
    let columnCount = cells[0].Length

    {RowCount = rowCount; ColumnCount = columnCount; Cells = cells}

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

let doWords () =
    let puzzle = "2001\n0..0\n1000\n0..0"
    let words = [|"casa"; "alan"; "ciao"; "anta"|]
    let crossword = MakeCrossword puzzle
    printfn "foo"

doWords ()

printfn "%b" (IsMatch "Hello  " "Hello")
printfn "%b" (IsMatch "HellO" "Hello")
printfn "Done"
