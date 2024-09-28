Require Import PeanoNat.
Require Import Lia.

Require Import Vector.
Definition vec (T : Type) (len : nat) : Type := 
    Vector.t T len.
Definition vnil {T : Type} :=
    Vector.nil T.
Definition vcons {T : Type} {n : nat} 
        (h : T) (t : vec T n) : vec T (S n) :=
    Vector.cons T h n t.
Local Notation "[ ]" := (nil _).
Local Notation "h :: t" := (cons _ h _ t).
Notation "[ x ]" := (cons _ x _ (nil _)).
Notation "[ x ; y ; .. ; z ]" :=
    (cons _ x _ (cons _ y _ .. (cons _ z _ (nil _)) ..)).

Inductive type : Type :=
| Int
| String
| Unit.

Inductive marker_type : Type :=
| Bool (b : bool) | Nat (n : nat).
Definition marker := vec marker_type.

Inductive gene : Type :=
| Empty
| Var (idx : nat)
| Fun {n_args : nat} 
    (arg_types : vec type n_args) (body : gene)
| Gene (g : gene)
| Marker {n} (m : marker n)
| SelfGene | SelfMarker.

Definition mutation : Type :=
    gene -> marker_type -> gene.

Definition genome (n m : nat) : Type :=
    (vec gene n) * (vec (marker n) m) * (vec mutation m).

Compute ([Fun [Int] (Var 0)], 
    [[Bool false]; [Nat 0]], 
    [fun _ _ => Empty; fun _ _ => Var 1]) : genome 1 2.

Definition vec_len {T : Type} {n : nat} (v : vec T n) : nat := 
    n.

Fixpoint vec_prod {T1 T2 : Type} {n : nat} 
    (v1 : vec T1 n) (v2 : vec T2 n) : vec (T1 * T2) n.
    assert (vec_len v1 = vec_len v2) by reflexivity.
    destruct v1 eqn:E1, v2 eqn:E2; inversion H.
    - exact [].
    - subst; exact ((h, h0) :: vec_prod T1 T2 n0 v v0).
Defined.

Fixpoint _mapi {A B n m} (v : vec A n) idx (H : idx < m)
        (H0 : 0 < m)
        (f : forall (x : nat) (H : x < m), A -> B)
        {struct v} : vec B n.
    destruct v.
    - exact [].
    - assert (m - idx - 1 < m) by lia.
      remember (f (m - idx - 1) H1 h).
        destruct idx as [| idx' ]. 
        -- specialize (_mapi A B n m v 0 H H0 f).
           exact (b :: _mapi).
        -- assert (idx' < m) by (transitivity (S idx'); auto).
            specialize (_mapi A B n m v idx' H2 H0 f).
            exact (b :: _mapi).
Defined.

Definition mapi {A B n} (f : forall (x : nat) (H : x < n), A -> B) 
    (v : vec A n) : vec B n.
    destruct (Nat.ltb 0 n) eqn:E.
    - apply Nat.ltb_lt in E.
      apply (@_mapi A B n n v (n - 1) ltac:(lia) ltac:(lia) f).
    - apply Nat.ltb_ge, Nat.le_0_r in E. subst.
      exact [].
Defined.

Definition render_genes {n m}
    (genes : vec gene n)
    (markers : vec (marker n) m)
    (mutations : vec mutation m)
    : vec gene n :=
    mapi (fun (idx : nat) (H : idx < n) (g : gene) =>
        fold_left (fun (acc : gene) mut_marks =>
            let '(mut, marks) := mut_marks in
            mut acc (nth_order marks H)
        ) g (vec_prod mutations markers)
    ) genes.

Definition silent : mutation := fun g m =>
    match m with
    | Bool true => Empty
    | _ => g
    end.

Compute render_genes
    [Var 1; Var 2; Var 3]
    [[Bool true; Bool true; Bool false]]
    [silent].

Definition render_genome 
        {n m : nat} (g : genome n m) : genome n m :=
    let '(genes, markers, mutations) := g in
    let genes' := render_genes genes markers mutations in
    (genes', markers, mutations).
