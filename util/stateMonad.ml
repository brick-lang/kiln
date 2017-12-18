open Basic
       
module type S = sig
  type state
  include Monadic.S

  val run : (state -> 'a * state) -> state -> 'a * state
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val eval : (state -> 'a * state) -> state -> 'a
  val exec : (state -> 'a * state) -> state -> state
end

module Make(K : sig type t end) (* : (S with type state := K.t) *) = struct (*  *)
  module Run = struct
    type 'a t = K.t -> 'a * K.t
  end
  include Run

  let run ma k = ma k

  module Monad = struct
    type 'a t = 'a Run.t
    let return (lazy a) = fun s -> (a, s)
    let bind (m : K.t -> 'a * K.t) (f : 'a -> K.t -> 'b * K.t ) : K.t -> 'b * K.t = fun s ->
      let (a, t) = m s
      in run (f a) t

    let map ma ~f = bind ma (fun a -> return @@ lazy (f a))
  end
  include Monadic.Make(Monad)

  let join maa = maa >>= fun ma -> ma
  let get = fun s -> (s,s)
  let put s = fun _ -> ((), s)
  let modify f = get >>= fun k -> put (f k)
  let eval act = fst *.* run act
  let exec act = snd *.* run act
end


