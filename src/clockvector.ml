type cv = int array

let fresh n = Array.make n 0

let size v = Array.length v

let get v i = Array.get v i

let set v i x = Array.set v i x

let string_of_cv cv =
    let s_of_el s el = s ^ (string_of_int el) ^ " " in
        (Array.fold_left s_of_el "[|" cv) ^ "|]"

exception MismatchedSizes

let max cv1 cv2 =
    if size cv1 = size cv2
    then
        let result = fresh (size cv1) in
        for i = 0 to (size cv1) - 1 do
            set result i (max (get cv1 i) (get cv2 i))
        done; result
    else raise MismatchedSizes
