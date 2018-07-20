type objectref = ObjectRef of int32
type arrayref = ArrayRef of int32

type count = Count of int32
type length = Length of int32
type 'a value = Value
type index_ref = int32 value

type _ op =
  | Aaload : arrayref op * index_ref op -> _ value op (** load onto the stack a reference from an array *)
  | Aastore : arrayref op * index_ref op * _ value op -> unit op (** store into a reference in an array *)
  | Aconst_null : objectref op (** push a null reference onto the stack *)
  | Aconst_null' : arrayref op (** push a null reference onto the stack *)
  | Aload : _ value -> objectref op (** load a reference onto the stack from a local variable #index *)
  | Aload_0 : objectref op (** load a reference onto the stack from local variable 0 *)
  | Aload_1 : objectref op  (** load a reference onto the stack from local variable 1 *)
  | Aload_2 : objectref op  (** load a reference onto the stack from local variable 2 *)
  | Aload_3 : objectref op  (** load a reference onto the stack from local variable 3 *)
  | Anewarray : count -> arrayref op  (** create a new array of references of length count and component type identified by the class reference index (indexbyte1 << 8 + indexbyte2) in the constant pool *)
  | Areturn : objectref op -> objectref op (** return a reference from a method [NOTE: This is consuming reference] *)
  | Arraylength : arrayref op -> length op (** get the length of an array *)
  | Astore : (* ???index??? *) objectref op -> unit op (** store a reference into a local variable #index *)
  | Astore_0 : objectref op -> unit op (** store a reference into local variable 0 *)
  | Astore_1 : objectref op -> unit op (** store a reference into local variable 1 *)
  | Astore_2 : objectref op -> unit op (** store a reference into local variable 2 *)
  | Astore_3 : objectref op -> unit op (** store a reference into local variable 3 *)
  | Athrow : objectref op -> objectref op (** throws an error or exception (notice that the rest of the stack is cleared, leaving only a reference to the Throwable) *)
  | Baload : arrayref op * index_ref op -> bool value op (** load a byte or Boolean value from an array *)
  | Bastore : arrayref op * index_ref op * bool value op -> unit value op (** store a byte or Boolean value from an array *)
  | Breakpoint : unit value op (** reserved for breakpoints in Java debuggers; should not appear in any class file *)
  | Caload : arrayref op * index_ref op -> char value op (** load a char from an array *)
  | Castore : arrayref op * index_ref op * char value op -> unit op (** store a char into an array *)

  | Iconst_m1 : int value op (** load the int value −1 onto the stack *)
  | Iconst_0 : int value op (** load the int value 0 onto the stack *)
  | Iconst_1 : int value op (** load the int value 1 onto the stack *)
  | Iconst_2 : int value op (** load the int value 2 onto the stack *)
  | Iconst_3 : int value op (** load the int value 3 onto the stack *)
  | Iconst_4 : int value op (** load the int value 4 onto the stack *)
  | Iconst_5 : int value op (** load the int value 5 onto the stack *)

  (*  ADDITIONAL combinators - NON JVM based - just so we can emulate stms *)
  | ExprFromStms : unit op * 'a op -> 'a op

let p a = Baload(Aconst_null',  a)

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

type len = Stdint.uint16 (*u2*)
type str = Stdint.uint8 list (*u1 list*)

type constant =
| CClass 
| CFiledRef
| CMethodRef
| CInterfaceMethodRef
| CString
| CInteger
| CFloat
| CLong
| CDouble
| CNameAndType
| CUtf8 of int * int list
| CMethodHandle
| CMethodType
| CInvokeDynamic

module List = struct
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
    (CUtf8 (len, str), r) |> Ok
  | 3 :: x -> failwith "todo"
  | x :: _ -> Printf.sprintf "usnuported constant tag = %i" x |> Error
  | [] ->Error "usnuported constant tag = <empty stream of data>"

let base_type_of_char: type a. char -> a baseType option = function
| 'B' -> Some BtByte
| 'C' -> Some BtChar
| _ -> None



let parse_class_file x =
  Ok x
  |> get_4u "missing magic number" (fun magic_number rem -> if magic_number = 0xCAFEBABE then Ok rem else Error "wrong magic number")
  |> get_2u "missing minor_version" (fun minor_version rem ->
    Ok rem |> get_2u "missing major_verion" (fun major_version rem ->
      let _ = {minor_version; major_version} in
      Ok rem))
  |> get_2u "missing const_pool_count" (fun constant_pool_count _rem -> Error (string_of_int constant_pool_count))


let () =
  read_file "/Users/kevin/_projects/java/HelloWorld.class" 
  |> parse_class_file
  |> function Ok _x -> print_endline "x" | Error x -> print_endline x