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

let rec sumAhead list=
    match list with
    h::_ when h<0 -> list |> List.fold (+) 0
    |_::t->  sumAhead t
    |[] -> 0

let rec sumAheadMyImp list=
    match list with
    h::_ when h<0 -> list |> ListFold (+) 0
    |_::t->  sumAhead t
    |[] -> 0


// TEST
sumAheadMyImp [3;3;-1;5;5;-2;6;4;-3;8;2] =24;;
sumAheadMyImp [1;2;3] = 0;;
sumAheadMyImp [] = 0;;
sumAhead [3;3;-1;5;5;-2;6;4;-3;8;2] =24;;
sumAhead [1;2;3] = 0;;
sumAhead [] = 0;;

// Ex3
let rec sumBack list=
    let rec helper listOfElem acc index=
        match listOfElem,index with 
            _,None-> 0
            |h::_,Some(0) -> List.foldBack (+) acc h
            |h::t,Some(i) -> helper t (h::acc) (Some(i-1)) 
            |_->0

    helper list []  (list |> List.tryFindIndexBack (fun x-> x<0))

let rec sumBackMyImp list=
    let rec helper listOfElem acc index=
        match listOfElem,index with 
            _,None-> 0
            |h::_,Some(0) -> ListFoldBack (+) acc h
            |h::t,Some(i) -> helper t (h::acc) (Some(i-1)) 
            |_->0

    helper list []  (list |> List.tryFindIndexBack (fun x-> x<0))
    
// TEST
sumBack [3;3;-1;5;5;-2;6;4;-3;8;2] = 20;;
sumBack [] = 0;;
sumBack [1;2;3] = 0;;
sumBackMyImp [3;3;-1;5;5;-2;6;4;-3;8;2] = 20;;
sumBackMyImp [] = 0;;
sumBackMyImp [1;2;3] = 0;;

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
 
let rec permutations list =
    let rec helper listToPermute setOfTaken=
        seq {
            if Set.count setOfTaken = List.length listToPermute then 
                yield []
            else 
                for element in listToPermute do 
                    if not(Seq.contains element setOfTaken) then 
                        for singleElem in helper list (Set.add element setOfTaken) do
                            yield element::singleElem 
        }

    helper list Set.empty;;

permutations ["ala"; "ma"; "kota"] |> Seq.toList;;
permutations ["ala"; "ma"; "kota"; "Filemona"] |> Seq.toList;;
