//Ex1
let rec ListFold func acc list=
    match list with
    h::t-> (ListFold func acc t) |> func h 
    |[]-> acc


// TEST
List.fold (+) 0 [1..10] = ListFold (+) 0 [1..10];;
List.fold (*) 0 [1..10] = ListFold (*) 0 [1..10];;

let rec ListFoldBack func list acc=
    match list with
    h::t-> (func acc h) |> ListFoldBack func t
    |[]-> acc

// TEST
ListFoldBack (+) [1..10] 0 = List.foldBack (+) [1..10] 0;;

// Ex 2
let SumAhead list=

    let rec sumFrom elem listOfElements=
        match listOfElements with
        h::t when h=elem -> t |> ListFold (+) elem
        |_::t-> sumFrom elem t
        |_-> 0 
        
    match list |> List.tryFind (fun x-> x<0) with
        Some(elem)-> list |> sumFrom elem
        |None-> 0



// List.fold_left (fun (s, p) h -> (s + h, p * h)) (0, 1) xs;;
SumAhead [3;3;-1;5;5;-2;6;4;-3;8;2] = 24
SumAhead []=0
SumAhead [1;2;3]=0
