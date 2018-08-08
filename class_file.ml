let read_file path : int list =
    let file = open_in_bin path in
    let data = ref [] in
    try
      while true do
        data := input_byte file :: !data
      done; !data
    with End_of_file -> !data
    |> List.rev

type file_version = {
  minor_version : int;
  major_version : int;
}

let cont f (error:string) cont' = function
  | Ok x -> begin
    match f x with
    | Some (value, remaining) -> cont' value remaining
    | None -> Error error end
  | Error e -> Error e

let get_2u = cont (function
  | x::y::z -> Some (x lsl 8 + y, z)
  | _ -> None)

let get_4u = cont (function
  | a::b::c::d::z -> Some ((a lsl 24) + (b lsl 16) + (c lsl 8) + d, z)
  | _ -> None)


type class_name = ClassName of string [@@deriving show]

(*
type _ baseType =
| BtByte : bytes baseType
| BtChar : char baseType
| BtDouble : float baseType
| BtFloat : float baseType (* not sure what type to use for single-precision floating point *)
| BtInteger : int32 baseType
| BtLong : int64 baseType
| BtShort : Stdint.int16 baseType
| BtBoolean : bool baseType
| BtReference : class_name -> objectref baseType
| BtArrayReference : arrayref baseType
*)

type base_type = BtByte | BtChar | BtDouble | BtFloat  | BtInteger | BtLong | BtShort | BtBoolean
                [@@deriving show]



type len = Stdint.uint16
type str = Stdint.uint8 list
type index = Stdint.uint16 [@printer fun fmt x -> fprintf fmt "%s" (Stdint.Uint16.to_string x)][@@deriving show]

type name_index = index [@@deriving show]
type class_index = index [@@deriving show]
type name_and_type_index = index [@@deriving show]
type string_index = index [@@deriving show]
type descriptor_index = index [@@deriving show]
type reference_index = index [@@deriving show]
type bootstrap_method_attr_index = index [@@deriving show]

type reference_kind =  REF_getField  | REF_getStatic | REF_putField | REF_putStatic | REF_invokeVirtual | REF_invokeStatic
                     | REF_invokeSpecial | REF_newInvokeSpecial | REF_invokeInterface [@@deriving show]

type class_type = JTObject of class_name | JTArray of class_type | JTBase of base_type [@@deriving show]


let string_to_class_type str =
    let list_to_string l = List.to_seq l |> String.of_seq  in
    let rec array_to_java_type = function
        | '[' :: xs -> array_to_java_type xs |> JTArray
        | 'L' :: xs -> 
            list_to_string xs |> ClassName |> JTObject
        | ['B'] -> JTBase BtByte
        | ['C'] -> JTBase BtChar
        | ['D'] -> JTBase BtDouble
        | ['F'] -> JTBase BtFloat
        | ['I'] -> JTBase BtInteger
        | ['J'] -> JTBase BtLong
        | ['S'] -> JTBase BtShort
        | ['Z'] -> JTBase BtBoolean
        | [] -> failwith (Printf.sprintf "string_to_java_type empty string - %s" str)
        | x -> failwith (Printf.sprintf "string_to_java_type unknown type - '%s' ('%s')" (list_to_string x) str) in
    let string_to_java_type = function
    | '[' :: xs -> array_to_java_type xs |> JTArray
    | _ -> JTObject (ClassName str)
    in string_to_java_type (String.to_seq str |> List.of_seq)

type constant =
| CClass of class_type
| CFiledRef of class_type * name_and_type_index
| CMethodRef of class_type * name_and_type_index
| CInterfaceMethodRef of class_type * name_and_type_index
| CString of string
| CInteger of int32
| CFloat of float
| CLong of int64
| CDouble of float (* this is 63bit type - should be 64... *)
| CNameAndType of name_index * descriptor_index
| CModule of name_index
| CPackage of name_index
| CUtf8 of string
| CMethodHandle of reference_kind * reference_index
| CMethodType of descriptor_index
| CInvokeDynamic of bootstrap_method_attr_index * name_and_type_index
| CEmpty
[@@deriving show]
let as_int = Stdint.Uint16.to_int


module List = struct
  include List
  let part i =
    let rec part' i acc l = 
      if i = 0 then acc |> List.rev, l
      else
        match l with
        | [] -> acc |> List.rev, []
        | x :: xs -> part' (i - 1) (x::acc) xs
    in part' i []
end

let (||>) (a:'a*'b) f = f a

let get_int16 a b = a lsl 8 + b
let get_uint16 a b = get_int16 a b |> Stdint.Uint16.of_int (*TODO fix*)
let get_int32 a b c d = 
    let (!@) = Int32.of_int in
    let (>>) = Int32.shift_left in
    let (+) = Int32.add in
    ((!@a >> 24) + (!@b >> 16) + (!@c >> 8) + !@d)

let get_int64 a b c d e f g h = 
    let (!@) = Int64.of_int in
    let (>>) = Int64.shift_left in
    let (+) = Int64.add in
    ((!@a >> 56) + (!@b >> 48) + (!@c >> 40) + (!@d >> 32) + (!@e >> 24) + (!@f >> 16) + (!@g >> 8) + !@h)

let try_get_constant = function
  | 1 :: x :: y :: r -> 
    let len = get_int16 x y in
    let (str, r) = r |> List.part len in
    (* this mapping will work only for non-null ASCII chars! *)
    (`CUtf8 (str |> List.map Char.chr |> List.to_seq |> String.of_seq), r) |> Ok

  | 3:: a :: b :: c :: d :: r -> Ok (`CInteger (get_int32 a b c d), r)
  | 4:: _a :: _b :: _c :: _d :: r -> Ok (`CFloat 0.0, r) (* TODO *)
  | 5:: a :: b :: c :: d :: e :: f :: g :: h :: r -> Ok (`CLong(get_int64 a b c d e f g h), r)
  | 6:: _a :: _b :: _c :: _d :: _e :: _f :: _g :: _h :: r -> Ok (`CDouble 0.0, r)  (* TODO *)
  | 7:: a :: b :: r -> Ok (`CClass (get_uint16 a b), r)
  | 8:: a :: b :: r -> Ok (`CString(get_uint16 a b), r)
  | 9:: a :: b :: d :: e :: r -> Ok (`CFiledRef(get_uint16 a b, get_uint16 d e), r)
  | 10:: a :: b :: d :: e :: r -> Ok (`CMethodRef(get_uint16 a b, get_uint16 d e), r)
  | 11:: a :: b :: d :: e :: r -> Ok (`CInterfaceMethodRef(get_uint16 a b, get_uint16 d e), r)
  | 12:: a :: b :: d :: e :: r -> Ok (`CNameAndType(get_uint16 a b, get_uint16 d e), r)
  | 15:: kind' :: a :: b :: r -> 
    let kind = match kind' with
               | 1 -> Some REF_getField
               | 2 -> Some REF_getStatic
               | 3 -> Some REF_putField
               | 4 -> Some REF_putStatic
               | 5 -> Some REF_invokeVirtual
               | 6 -> Some REF_invokeStatic
               | 7 -> Some REF_invokeSpecial
               | 8 -> Some REF_newInvokeSpecial
               | 9 -> Some REF_invokeInterface
               | _ -> None               
    in begin match kind with
    | Some kind -> Ok (`CMethodHandle(kind, get_uint16 a b), r)
    | None -> Printf.sprintf "unknown kind = %i" kind' |> Error end
  | 16:: a :: b :: r -> Ok (`CMethodType(get_uint16 a b), r)
  | 18:: a :: b :: d :: e :: r -> Ok (`CInvokeDynamic(get_uint16 a b, get_uint16 d e), r)
  | 19:: a :: b :: r -> Ok (`CModule(get_uint16 a b), r)
  | 20:: a :: b :: r -> Ok (`CPackage(get_uint16 a b), r)
  | x :: _ -> 
    Printf.sprintf "unsuported constant tag = %i" x |> Error
  | [] ->Error "usnuported constant tag = <empty stream of data>"

let try_get_constants no (data:int list) =
    let rec try_get_constants no (data:int list) =
        if no = 0 then Ok([], data)
        else
            match try_get_constant data with
            | Ok (result, remaining) ->
                let results =
                    match result with
                    | `CDouble _ | `CLong _ -> 
                        (*
                         If a CONSTANT_Long_info or CONSTANT_Double_info structure is the item in the constant_pool table at index n, 
                         then the next usable item in the pool is located at index n+2. The constant_pool index n+1 must be valid but is considered unusable.

                         In retrospect, making 8-byte constants take two constant pool entries was a poor choice.
                        *)
                        [result; `Null]
                    | x -> [x] in
                begin match try_get_constants (no - (List.length results)) remaining with
                | Ok (tail, remaining) -> Ok (results @ tail, remaining)
                | Error error -> Error error end
            | Error e -> Error e
    in 
        match try_get_constants no data with
        | Ok (result, remaining) ->
            let data = result |> List.mapi (fun i x -> i, x) in
            let module IntMap = Core.Int.Map in
            let map = IntMap.of_alist_exn data in
            let uint16_to_int (x:Stdint.uint16) = Stdint.Uint16.to_int x in
            let get_string_from_map x =
                match IntMap.find map ((uint16_to_int x) - 1) with
                        | Some (`CUtf8 x) -> x
                        | Some _-> failwith "[CUtf8] Corresponding index has wrong type..." 
                        | None -> failwith "[CUtf8] Cannot find corresponding index..." in
            let get_class_info_from_map x =
                match IntMap.find map ((uint16_to_int x) - 1) with
                        | Some (`CClass x) -> x
                        | Some _ -> failwith "[CClass] Corresponding index has wrong type..." 
                        | None -> failwith "[CClass] Cannot find corresponding index..." in
            let get_class_info_from_class_index index = 
                string_to_class_type (get_string_from_map (get_class_info_from_map index)) in
            IntMap.map map ~f: (function 
                | `CUtf8 x -> CUtf8 x
                | `CInteger x -> CInteger x
                | `CFloat x -> CFloat x
                | `CLong x -> CLong x
                | `CDouble x -> CDouble x
                | `CClass x -> CClass (string_to_class_type (get_string_from_map x))
                | `CString x -> CString (get_string_from_map x)
                | `CFiledRef (class_index,b) -> CFiledRef (get_class_info_from_class_index class_index,b)
                | `CMethodRef (class_index,b) -> CMethodRef (get_class_info_from_class_index class_index,b)
                | `CInterfaceMethodRef (class_index,b) -> CInterfaceMethodRef (get_class_info_from_class_index class_index,b)
                | `CNameAndType (a,b) -> CNameAndType (a,b)
                | `CMethodHandle (a,b) -> CMethodHandle (a,b)
                | `CInvokeDynamic (a,b) -> CInvokeDynamic (a,b)
                | `CMethodType x -> CMethodType x
                | `CModule x -> CModule x
                | `CPackage x -> CPackage x
                | `Null -> CEmpty)
            |> fun x ->  Ok (x, remaining)
        | Error e -> Error e
        
(* let base_type_of_char: type a. char -> a baseType option = function
| 'B' -> Some BtByte
| 'C' -> Some BtChar
| _ -> None *)



let parse_class_file x =
  Ok x
  |> get_4u "missing magic number" (fun magic_number rem -> if magic_number = 0xCAFEBABE then Ok rem else Error "wrong magic number")
  |> get_2u "missing minor_version" (fun minor_version rem ->
    Ok rem |> get_2u "missing major_verion" (fun major_version rem ->
      let _ = {minor_version; major_version} in
      Ok rem))
  |> get_2u "missing const_pool_count" (fun constant_pool_count rem -> 
        match try_get_constants (constant_pool_count - 1) rem with
        | Ok (x, _) -> x |> Core.Int.Map.data |> List.map show_constant |> String.concat "\n" |> Error
        | Error x -> Error x)
