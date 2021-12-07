// Ex2
let fst tuple=
    match tuple with
        (first,_,_)-> first;;

let mid tuple=
    match tuple with
        (_,mid,_)-> mid;;
        
let lst tuple=
    match tuple with
        (_,_,last)-> last;;


fst (1,2,3)=1;;
mid (1,2,3)=2;;
lst (1,2,(1,3))=(1,3);;

// Ex 3
// a
let rec factorial n=
    if n<=0 then 1
    else n*factorial(n-1);;

factorial 5 = 120;;
// b
let rec binomialCoefficient n k= 
    if (n>=0 && k>=0) then factorial(n)/(factorial(k)*factorial(n-k))
    else 0;;
binomialCoefficient 4 1 = 4;;
binomialCoefficient 10 2 = 45;;

// c
let rec pascal(n: int64): int64 list=
    if n=0L then [1L]
    else if n=1L then [1L;1L]
    else
        let rec zipp xs = 
            match xs with
            _::t when t=[] -> [(1L,0L)]
            |h1::h2::_-> (h1,h2)::zipp(List.tail xs)
            |_->[]
        in
        let rec helper(pascalList: int64 list, n:int64): int64 list =
            if n>1L then helper(1L::List.map (fun (x,y) ->x+y) (zipp(pascalList)),n-1L)
            else pascalList
        in
        helper([1L;2L;1L],n-1L)
    
pascal(0L)=             [1L];;
pascal(1L)=            [1L;1L];;
pascal(2L)=          [1L;2L;1L];;
pascal(3L)=       [1L; 3L; 3L; 1L];;
pascal(4L)=     [1L; 4L; 6L; 4L; 1L];;
pascal(5L)= [1L; 5L; 10L; 10L; 5L; 1L];;
pascal(6L)=[1L; 6L; 15L; 20L; 15L; 6L; 1L];;

//Ex4
// Faculty should be a Discriminated Union of PPT, IZ, IT, EFM, MAT
type Faculty = 
    |PPT 
    |IZ
    |IT
    |EFM
    |MAT;;

type Student = { StudentName: string; GPA: float; Faculty: Faculty};;



let students = [
    {StudentName= "Kamil"   ;GPA= 98.0;     Faculty= IZ};
    {StudentName= "Patryk"  ;GPA= 43.4;     Faculty= EFM};
    {StudentName= "Olek"    ;GPA= 11.2;     Faculty= MAT};
    {StudentName= "Adam"    ;GPA= 32.4;     Faculty= IT};
    {StudentName= "Jasia"   ;GPA= 100.0;    Faculty= MAT};
    {StudentName= "Kasia"   ;GPA= 40.2;     Faculty= IZ};
    {StudentName= "Ala"     ;GPA= 11.0;     Faculty= PPT};
    {StudentName= "Basia"   ;GPA= 98.0;     Faculty= IT};
    {StudentName= "Norbert" ;GPA= 33.0;     Faculty= PPT};
    {StudentName= "Jakub"   ;GPA= 21.31;    Faculty= EFM};
]




let rec printITGuy listOfStudents = 
    let rec helper students itStudents=
        match students with
            h::t when h.Faculty=IT -> helper t (h::itStudents);
            |_::t-> helper t itStudents
            |[]->itStudents
    in 
    helper listOfStudents [];;

;;

printITGuy students =[{ StudentName = "Basia" ;GPA = 98.0 ;Faculty = IT }; { StudentName = "Adam" ; GPA = 32.4 ;Faculty = IT }];;


let averageGPA listOfStudents faculty =
    let rec helper students gpaPoints=
        match students with
            h::t when h.Faculty=faculty-> helper t (h.GPA::gpaPoints);
            |_::t-> helper t gpaPoints
            |[]-> (List.fold (+) 0. gpaPoints)/float(gpaPoints.Length)
    in 
    helper listOfStudents [];;

averageGPA students IZ=69.1;;