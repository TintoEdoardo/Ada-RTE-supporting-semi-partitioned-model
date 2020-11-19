# Scenario 4
Torna all'[indice](../index.md)

Questo scenario è semplicemente una verifica dello [scenario 3](../scenario_3/scenario_3.md) nel caso che i tasks ad eccedere il loro budget siano più di uno.

- [X] Allocare il seguente task set
```
   P1 : Periodic_First_CPU (Pri => 10, Budget => 200_000, Workload => 1, Period => 300_000);
   BE1 : BE_First_CPU (Pri => 20, Budget => 100_000, Workload => 50_000, Period => 800_000);
   BE2 : BE_First_CPU (Pri => 30, Budget => 100_000, Workload => 50_000, Period => 1_000_000);
```
-  [X] `P1` non forzerà mai un `CPU_Budget_Exceeded`.
-  [X] Devono essere rilevati due `CPU_Budget_Exceeded`, uno relativo a `BE1` e uno relativo a `BE2`
-  [X] i due tasks `BE1` e `BE2` devono trovarsi nella `Discarded_Thread_Table`

Torna all'[indice](../index.md)