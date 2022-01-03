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

//Ex 4
let countNonWhiteChar list=
    list 
    |> List.filter (fun x-> not(System.String.IsNullOrWhiteSpace(x))) 
    |> List.map (fun x-> x.Trim()) 
    |> List.mapi (fun i x -> i+ String.length x )
    |> List.fold (+) 0

countNonWhiteChar [" ala";"";"ma ";" ";"kota";" "]=12;;
countNonWhiteChar [" "]=0;;
countNonWhiteChar []=0;;
countNonWhiteChar ["";" "]=0;;

// Ex 5

let permutation list=


