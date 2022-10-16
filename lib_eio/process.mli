type status = Exited of int | Signaled of int | Stopped of int

val pp_status : Format.formatter -> status -> unit

class virtual t : object
    inherit Generic.t
    method virtual pid : int
    method virtual status : status Promise.t
    method virtual stop : unit
end

val pid : #t -> int
(** The process ID *)

val status : #t -> status Promise.t
(** Checks the status of the subprocess, this will block waiting for the subprocess
    to terminate or be stopped by a signal. *)

val stop : #t -> unit
(** Stop a running subprocess *)

class virtual mgr : object
    inherit Generic.t
    method virtual spawn : 
        sw:Switch.t -> 
        ?cwd:Fs.dir Path.t ->
        stdin:Flow.source ->
        stdout:Flow.sink -> 
        stderr:Flow.sink ->
        string ->
        string list ->
        t
    
    method virtual spawn_detached :
        ?cwd:Fs.dir Path.t ->
        stdin:Flow.source ->
        stdout:Flow.sink -> 
        stderr:Flow.sink ->
        string ->
        string list ->
        t
end
(** A process manager capable of spawning new processes. *)

val spawn : sw:Switch.t -> #mgr -> ?cwd:Fs.dir Path.t -> stdin:Flow.source -> stdout:Flow.sink -> stderr:Flow.sink -> string -> string list -> t
(** [spawn ~sw t cmd] creates a new subprocess that is connected to the
    switch [sw]. The standard input and outputs redirect to nothing by default. *)

val spawn_detached : #mgr -> ?cwd:Fs.dir Path.t -> stdin:Flow.source -> stdout:Flow.sink -> stderr:Flow.sink -> string -> string list -> t
(** [spawn_detached t cmd] is like {! spawn} but the new subprocess is not
    attached to any switch. *)