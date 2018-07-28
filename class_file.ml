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
| CClass of name_index
| CFiledRef of class_index * name_and_type_index
| CMethodRef of class_index * name_and_type_index
| CInterfaceMethodRef of class_index * name_and_type_index
| CString of string_index
| CInteger of int32
| CFloat of float
| CLong of int64
| CDouble of float (* this is 63bit type - should be 64... *)
| CNameAndType of name_index * descriptor_index
| CUtf8 of string
| CMethodHandle of reference_kind * reference_index
| CMethodType of descriptor_index
| CInvokeDynamic of bootstrap_method_attr_index * name_and_type_index

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
    (str |> List.map Char.chr |> List.to_seq |> String.of_seq |> CUtf8, r) |> Ok

  | 3:: a :: b :: c :: d :: r -> Ok (get_int32 a b c d |> CInteger, r)
  | 4:: _a :: _b :: _c :: _d :: r -> Ok (CFloat 0.0, r) (* TODO *)
  | 5:: a :: b :: c :: d :: e :: f :: g :: h :: r -> Ok (get_int64 a b c d e f g h |> CLong, r)
  | 6:: _a :: _b :: _c :: _d :: _e :: _f :: _g :: _h :: r -> Ok (CDouble 0.0, r)  (* TODO *)
  | 7:: a :: b :: r -> Ok (get_uint16 a b |> CClass, r)
  | x :: _ -> Printf.sprintf "usnuported constant tag = %i" x |> Error
  | [] ->Error "usnuported constant tag = <empty stream of data>"

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
  |> get_2u "missing const_pool_count" (fun constant_pool_count _rem -> Error (string_of_int constant_pool_count))

