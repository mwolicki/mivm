open Types
open Class_file

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

let () =

  let path = "/Users/kevin/_projects/java" in
  let files = Sys.readdir path
    |> Array.to_list
    |> List.filter (fun x->(Filename.extension x) = ".class")
    |> List.map (Filename.concat path)
  in 
    files 
    |> List.iter (fun file ->
          Printf.printf "\n\n#####################\n\nparsing file %s\n\n#####################\n\n" file;
          read_file file
          |> parse_class_file
          |> function Ok _x -> print_endline "x" | Error x -> print_endline x)