Theorem simple: forall P: Prop, P -> P.
intros.
apply H.
Qed.

Theorem K : forall P Q: Prop, P -> Q -> P.
intros.
exact (simple P H).
Qed.

Theorem S : forall P Q R:Prop, (P -> Q -> R) -> (P -> Q) -> P -> R.
intros.
exact (H H1 (H0 H1)).
Qed.

Definition I := fun P: Prop => S P (P -> P) P (K P (P ->P)) (K P P).
