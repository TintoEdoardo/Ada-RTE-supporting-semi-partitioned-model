# Scenario 9

Torna all'[indice](../index.md)

Da questo scenario in poi, si introduce gradualmente l'utilizzo del secondo core. In questo, ci si preoccupa di verificare che il secondo core sia funzionante, cioè che:
- sia possibile allocare su di esso dei tasks;
- che essi riescano ad eseguire.

*Nomenclatura*
- con *CPU1* si intende il primo core;
- con *CPU2* si intende il secondo core.
  
In HW, in realtà, la numerazione parte da zero (quindi CPU0 e CPU1). Tuttavia, nel runtime Ravenscar di AdaCore, tale numerazione parte da 1. Nell'esporre gli scenari, si utilizzerà quest'ultimo modo per evitare il più possibile inconsistenze ed errori "sciocchi".

**N.B.**: in questo scenario, **nessun** task deve essere soggetto a *budget monitoring*.

Bisogna:
- [X] modificare l'interfaccia dei tipi task in maniera tale da poter specificare a quale CPU devono essere staticamente allocati;
```
  --  Periodic LO-CRIT task
  task type Low_Crit
      (Pri                : System.Priority;
      Low_Critical_Budget : Natural;
      Is_Migrable         : Boolean;
      Workload            : Positive;
      Period              : Positive;
      CPU_Id              : CPU)
  is
      pragma Priority (Pri);
      pragma CPU (CPU_Id);
  end Low_Crit;

  --  Periodic HI-CRIT task
  task type High_Crit
      (Pri                 : System.Priority;
      Low_Critical_Budget  : Natural;
      High_Critical_Budget : Natural;
      Workload             : Positive;
      Period               : Positive;
      CPU_Id               : CPU) 
  is
      pragma Priority (Pri);
      pragma CPU (CPU_Id);
  end High_Crit;
```
- [X] allocare il seguente taskset;
```
   HC1 : High_Crit
      (Pri => 40, Low_Critical_Budget => 200_000, High_Critical_Budget => 1_500_000,
      Workload => 1, Period => 1_000_000, CPU_Id => CPU'First);

   HC2 : High_Crit
      (Pri => 30, Low_Critical_Budget => 200_000, High_Critical_Budget => 1_500_000,
      Workload => 1, Period => 3_000_000, CPU_Id => CPU'Last);

   LC1 : Low_Crit
      (Pri => 20, Low_Critical_Budget => 200_000, Is_Migrable => True,
      Workload => 1, Period => 300_000, CPU_Id => CPU'First);

   LC2 : Low_Crit
      (Pri => 10, Low_Critical_Budget => 200_000, Is_Migrable => True,
      Workload => 1, Period => 200_000, CPU_Id => CPU'Last);
```

- [X] verificare che:
  - [X] `HC1` e `LC1`:
    - [X] siano allocati alla CPU1 e che (ri-)eseguano secondo il loro periodo;
    - [X] scrivano correttamente sulla porta seriale UART;
  - [X] `HC2` e `LC2`:
    - [X] siano allocati alla CPU2 e che (ri-)eseguano secondo il loro periodo;
    - [X] scrivano correttamente sulla porta seriale UART;

Vai al [prossimo scenario](../scenario_10/scenario_10.md).\
Torna all'[indice](../index.md).