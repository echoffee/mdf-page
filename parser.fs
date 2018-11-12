module parser

open System
open System.IO
open System

let (|Prefix|_|) (pref : string) (suf : string) = if suf.StartsWith(pref) then Some(suf.Substring(String.length pref)) else None;;
type spec = Title1 | Title2 | Title3 | Text
type input = {
    Type : spec
    Content : string
}

let addHTMLTags (s : string) = "<html><body>" + s + "</body></html>"

let convertToHTML (s : input) =
    if s.Content = "" then "" else
        match s.Type with 
            | Title1    -> "<h1>" + s.Content + "</h1>"
            | Title2    -> "<h2>" + s.Content + "</h2>"
            | Title3    -> "<h3>" + s.Content + "</h3>"
            | Text      -> "<p class='text'>" + s.Content + "</p>"

let rec determineTitleLevel n (x : string) =
    match x with
        | Prefix "#" t -> determineTitleLevel (n + 1) t
        | Prefix " " t -> Some n
        | _ -> None;;

let getTitle r = 
    let deep = determineTitleLevel 1 r
    match deep with
        | Some 1 -> Title1
        | Some 2 -> Title2
        | Some _ -> Title3
        | None   -> Text 

let rec getTitleContent s =
    match s with
        | Prefix "#" t -> getTitleContent t
        | Prefix " " t -> t
        | t -> t

let parse_seq se =
    let parse_in s =
        match s with
            | Prefix "#" r  -> { Type = getTitle r; Content = getTitleContent r}
            | _             -> { Type = Text; Content = s}
    let t = Seq.map (parse_in) se
    let f = Seq.fold(fun res x ->
        match (res, x) with
            | ([], x) -> [x]
            | (hd :: tl, x) -> if x.Type = hd.Type && x.Content <> "" then {Type = x.Type; Content = if hd.Content = "" then x.Content else hd.Content + "<br/>" + x.Content} :: tl else x :: res ) []
    List.rev(t |> f)

let rec length l = 
    match l with
        | [] -> 0
        | _ :: t -> 1 + length t;;

[<EntryPoint>]
let main args = 
    if length (Array.toList args) = 1 then 
        let lines = System.IO.File.ReadLines (Array.head args)
        let inputs = parse_seq lines
        let tags = Seq.map(convertToHTML) inputs
        let s = tags |> Seq.fold(fun res x -> res + x) ""
        printfn "%s" (addHTMLTags s)
        0 
    else 
        failwith "wrong number of arguments"
