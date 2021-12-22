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
    let firstSum= list |> ListFold (fun h acc -> if(h<0) then acc else acc+h) 0
    let wholeSum = list |> ListFold (+) 0
    wholeSum,firstSum
// List.fold_left (fun (s, p) h -> (s + h, p * h)) (0, 1) xs;;
SumAhead [3;3;-1;5;5;-2;6;4;-3;8;2]
SumAhead []
SumAhead [1;2;3]

//Ex 4
let countNonWhiteChar list=

    let newList = 
        list 
        |> List.filter (fun x-> not(System.String.IsNullOrWhiteSpace(x))) 
        |> List.map (fun x-> x.Trim()) 
        |> List.mapi (fun i x -> i+ String.length x )
        |> List.fold (+) 0
        
    newList

    
countNonWhiteChar [" ala";"";"ma ";" ";"kota";" "]=12;;
countNonWhiteChar [" "]=0;;
countNonWhiteChar []=0;;
countNonWhiteChar ["";" "]=0;;



