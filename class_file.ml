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

let try_get_constant = function
  | 1 :: x :: y :: r -> 
    let len = x lsl 8 + y in
    let (str, r) = r |> List.part len in
    (* (CUtf8 (Stdint.Uint16.of_int len, str |> List.map Stdint.Uint8.of_int), r) |> Ok *)
    failwith "todo"
  | 3 :: _x -> failwith "todo"
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

