open Types

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


type class_name = ClassName of string


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

type len = Stdint.uint16
type str = Stdint.uint8 list
type index = Stdint.uint16
type name_index = index
type class_index = index
type name_and_type_index = index
type string_index = index
type descriptor_index = index
type reference_index = index
type bootstrap_method_attr_index = index

type reference_kind = 
| REF_getField 
| REF_getStatic
| REF_putField
| REF_putStatic
| REF_invokeVirtual
| REF_invokeStatic
| REF_invokeSpecial
| REF_newInvokeSpecial
| REF_invokeInterface


type constant =
| CClass of string
| CFiledRef of class_index * name_and_type_index
| CMethodRef of class_index * name_and_type_index
| CInterfaceMethodRef of class_index * name_and_type_index
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

let as_int = Stdint.Uint16.to_int

let to_str_constant = function
| CClass name_index -> 
    Printf.sprintf "CClass (name_index = %s)" (name_index)
| CFiledRef (class_index, name_and_type_index) -> 
    Printf.sprintf "CFiledRef (class_index = %i, name_and_type_index = %i)" (as_int class_index) (as_int name_and_type_index)
| CMethodRef (class_index, name_and_type_index) -> 
    Printf.sprintf "CMethodRef (class_index = %d, name_and_type_index %d)" (as_int class_index) (as_int name_and_type_index)
| CInterfaceMethodRef (class_index, name_and_type_index) -> 
    Printf.sprintf "CInterfaceMethodRef (class_index = %d, name_and_type_index %d)" (as_int class_index) (as_int name_and_type_index)
| CString str -> 
    Printf.sprintf "CString (str = %s)" str
| CInteger int32 -> 
    Printf.sprintf "CInteger %s" (Int32.to_string int32)
| CFloat float -> 
    Printf.sprintf "CFloat %f" float
| CLong int64 -> 
    Printf.sprintf "CLong %s" (Int64.to_string int64)
| CDouble float  -> 
    Printf.sprintf "CDouble %f" float
| CNameAndType (name_index, descriptor_index) -> 
    Printf.sprintf "CNameAndType (name_index = %d, descriptor_index = %d)" (as_int name_index) (as_int descriptor_index)
| CModule name_index -> 
    Printf.sprintf "CModule (name_index = %d,)" (as_int name_index)
| CPackage name_index -> 
    Printf.sprintf "CPackage (name_index = %d,)" (as_int name_index)
| CUtf8 s -> 
    Printf.sprintf "CUtf8 %s" s
| CMethodHandle (_reference_kind, reference_index) -> 
    Printf.sprintf "CMethodHandle (KIND, reference_index = %d)"  (as_int reference_index)
| CMethodType descriptor_index -> 
    Printf.sprintf "CMethodType descriptor_index = %d" (as_int descriptor_index)
| CInvokeDynamic (bootstrap_method_attr_index, name_and_type_index) -> 
    Printf.sprintf "CInvokeDynamic (bootstrap_method_attr_index = %d, name_and_type_index = %d)" (as_int bootstrap_method_attr_index) (as_int name_and_type_index)
| CEmpty -> "CEmpty"
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
            let uint16_to_int = Stdint.Uint16.to_int in
            let get_string_from_map x =
                match IntMap.find map ((uint16_to_int x) - 1) with
                        | Some (`CUtf8 x) -> x
                        | Some _-> failwith "CString corresponding index has wrong type..." 
                        | None -> failwith "CString cannot find corresponding index..." in
            IntMap.map map ~f: (function 
                | `CUtf8 x -> CUtf8 x
                | `CInteger x -> CInteger x
                | `CFloat x -> CFloat x
                | `CLong x -> CLong x
                | `CDouble x -> CDouble x
                | `CClass x -> CClass (get_string_from_map x)
                | `CString x -> CString (get_string_from_map x)
                | `CFiledRef (a,b) -> CFiledRef (a,b)
                | `CMethodRef (a,b) -> CMethodRef (a,b)
                | `CInterfaceMethodRef (a,b) -> CInterfaceMethodRef (a,b)
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
        | Ok (x, _) -> x |> Core.Int.Map.data |> List.map to_str_constant |> String.concat "\n" |> Error
        | Error x -> Error x)
