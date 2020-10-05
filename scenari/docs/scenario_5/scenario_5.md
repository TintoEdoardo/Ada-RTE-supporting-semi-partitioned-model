# Scenario 5

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 4](../scenario_4/scenario_4.md), riguarda il:
- rappresentare il livello di criticità di un tasks.
- rappresentare se un certo tasks è migrabile o meno.
- rappresentare le varie modalità, in relazione al livello di criticità, di esecuzione di un core.
  - i livelli sono solo due: *high critical* e *low critical* 
- A tempo d'esecuzione, associare e aggiornare opportunamente il corretto livello di criticità del core.

Bisogna:
- [X] rappresentare il livello di criticità di un task.
  - [X] ``type Criticality is (HIGH, LOW);``
  - [X] il descrittore del task dovrà avere un campo apposito che, una volta impostato, **non** può essere più modificato.
- [X] rappresentare la migrabilità un task
  - [X] è sufficiente che il descrittore contenga un flag `Is_Migrabile`. 
- [X] rappresentare le modalità di esecuzione di un core
  - [X] ``type Mode is new Mixed_Criticality_System.Criticality;`` 

D'ora in avanti, i tasks con un livello di criticità *high* verranno chiamati *HI-CRIT*, mentre quelli con un livello *low* verrano chiamati *LO-CRIT*.

A tempo d'esecuzione:
- [X] all'avvio, la modalità di esecuzione del core è impostata a `LOW`.
- [X] se viene rilevato un `CPU_Budget_Exceeded`
  - [X] se è stato causato da un task *HI-CRIT*
    - [X] la modalità di esecuzione del core deve essere impostata ad `HIGH`.
    - [X] tutti i tasks *LO-CRIT* e migrabili devono essere inseriti nella `Discarded_Thread_Table`.
  - [ ] se è stato causato da un task *LO-CRIT*
    - [ ] che si fa? Si controlla se c'è un task ready a priorità più alta (=> `Context_Switch_Needed`) e in tal caso si fa context switch? 
    - [ ] si rimette il task nella coda dei sospesi e lo si sveglia al prossimo rilascio? Non avrebbe molto senso.
    - [ ] si mette il task a dormire per un po'? Quanto? 

Verificare il comportamento appena descritto con questi task set.
N.B. I tasks di tipo `Periodic_First_CPU` non causano mai `CPU_Budget_Exceeded`.
### Task set 1
```
P1 : Periodic_First_CPU
    (Pri => 10, Budget => 200_000, Criticality => LOW,
     Is_Migrabile => True, Workload => 1, Period => 300_000);

P2 : Periodic_First_CPU
    (Pri => 20, Budget => 200_000, Criticality => LOW,
     Is_Migrabile => False, Workload => 1, Period => 500_000);

P3 : Periodic_First_CPU
    (Pri => 30, Budget => 200_000,
     Criticality => HIGH, Workload => 1, Period => 700_000);

BE1 : BE_First_CPU
    (Pri => 10, Budget => 200_000,
     Criticality => HIGH, Workload => 100_000, Period => 2_000_000);
```

- [X] Dopo che il `CPU_Budget_Exceeded` causato da `BE1` è stato rilevato è gestito, `P1` deve trovarsi nella `Discarded_Thread_Table`. `P2` e `P3`, essendo rispettivamente non migrabile e *HI-CRIT*, non devono essere scartati.

### Task set 2

```
P1 : Periodic_First_CPU
    (Pri => 10, Budget => 200_000, Criticality => LOW,
     Is_Migrabile => True, Workload => 1, Period => 300_000);

P2 : Periodic_First_CPU
    (Pri => 20, Budget => 200_000, Criticality => LOW,
     Is_Migrabile => True, Workload => 1, Period => 500_000);

P3 : Periodic_First_CPU
    (Pri => 30, Budget => 200_000,
     Criticality => HIGH, Workload => 1, Period => 700_000);

BE1 : BE_First_CPU
    (Pri => 10, Budget => 200_000,
     Criticality => HIGH, Workload => 100_000, Period => 2_000_000);
```
- [X] Questa volta, alla rilevazione e gestione del `CPU_Budget_Exceeded` causato da `BE1`, i tasks `P1` e `P2` devono trovarsi nella `Discarded_Thread_Table`.

Torna all'[indice](../index.md)