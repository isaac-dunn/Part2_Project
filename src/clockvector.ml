class clockvector n = object
    val vector = Array.make n 0
    method size = n
    method get i = Array.get vector i
    method set i x = Array.set vector i x
    method print =
        for i = 0 to n - 1 do print_string ((string_of_int (Array.get vector i)) ^ " ") done
end;;

exception MismatchedVectorLengths;;

let max_cv cv1 cv2 =
    if cv1#size = cv2#size then
        let result = new clockvector (cv1#size) in
        let max (a, b) = if a > b then a else b in
            for i = 0 to cv1#size - 1 do
                result#set i (max(cv1#get i, cv2#get i))
            done; result
    else raise MismatchedVectorLengths;;

let x = new clockvector 3;;
let y = new clockvector 3;;
x#print;;
print_newline();;
x#set 1 7;;
x#print;;
print_newline();;
y#set 0 4;;
y#set 1 4;;
y#print;;
print_newline();;
let z = max_cv x y;;
z#print;;
print_newline();;
let omega = new clockvector 0;;
max_cv x omega;;
