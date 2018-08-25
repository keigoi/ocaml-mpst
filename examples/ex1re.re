open Mpst.ThreeParty;
open Mpst.ThreeParty.Shmem;
let (>>=) = Lwt.(>>=);
let return = Lwt.return;

/* A global protocol between A, B, and C */

let create_g : unit =>
  /* will be inferred by OCaml typechecker */
  mpst({.a: sess(branch((c, [>`msg(int, sess(select((b, {. left:  unit => sess(branch((b, [>`msg(string, sess(close))]))),
                                                           right: unit => sess(branch((b, [>`msg(int,    sess(branch((c, [>`msg(string, sess(close))]))))])))}))))]))),
         b: sess(branch((a, [>`left (unit, sess(select((c, {. right: unit => sess(select((a, {.msg:  string=>sess(close)})))}))))
                             |`right(unit, sess(select((a, {. msg:   int  => sess(select((c, {.left: unit=>sess(close)})))}))))]))),
         c: sess(select((a, {. msg:int => sess(branch((b, [>`left (unit, sess(select((a, {.msg: string => sess(close)}))))
                                                           |`right(unit,   sess(close))])))})))
    })
  = () =>
  (c --> a)   (msg()) @@
  (a -%%-> b) (leftright(),
    ~l1=((a, b),
         (b --> c) (right()) @@
         (b --> a) (msg()) @@
         finish),
    ~l2=((a, b),
         (b --> a) (msg()) @@
         (b --> c) (left()) @@
         (c --> a) (msg()) @@
         finish));

let (pa, pb, pc) = {
  let g = create_g();
  (get_sess(a, g), get_sess(b, g), get_sess(c, g));
};

/* participant A */
let t1: Lwt.t(unit) = {
  let s = pa;

  receive(C, s) >>= (`msg(x, s)) => {
    if (x == 0) {
      let s = send(B, x=>x#left, (), s);
      receive(B, s) >>= (`msg(str, s)) => {
        Printf.printf("A) B says: %s\n", str);
        close(s);
        return();
      }
    } else {
      let s = send(B, x => x#right, (), s);
      receive(B, s) >>= (`msg(x, s)) =>
        receive(C, s) >>= (`msg(str, s)) => {
          Printf.printf("A) B says: %d, C says: %s\n", x, str);
          close(s);
          return();
      }
    }
  } >>= () => {
      print_endline("A finished.");
      return()
  }
};

/* participant B */
let t2: Lwt.t(unit) = {
  let s = pb;
  receive(A, s) >>= fun
  | `left(_, s) => {
        let s = send(C, x => x#right, (), s);
        let s = send(A, x => x#msg, "Hooray!", s);
        close(s);
        return();
  }
  | `right(_, s) => {
        let s = send(A, x => x#msg, 1234, s);
        let s = send(C, x => x#left, (), s);
        close(s);
        return();
  } >>= () => {
    print_endline("B finished.");
    return();
  }
};

/* participant C */
let t3: Lwt.t(unit) = {
  let s = pc;
  print_endline("C: enter a number (positive or zero or negative):");
  Lwt_io.read_line(Lwt_io.stdin) >>= line => {
      let num = int_of_string(line);
      let s = send(A, x => x#msg, num, s);
      receive(B, s) >>= fun
      | `left(_, s) => {
        let s = send(A, x => x#msg, "Hello, A!", s);
        close(s);
        return();
      }
      | `right(_, s) => {
        close(s);
        return();
      } >>= () => {
        print_endline("C finished.");
        return();
      }
  };
};

let () = Lwt_main.run(Lwt.join([t1, t2, t3]));
