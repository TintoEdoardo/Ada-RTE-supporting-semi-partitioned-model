# Scenario 3

Torna all'[indice](../index.md)

Questo scenario, costruito a partire dallo [scenario 2](../scenario_2/scenario_2.md), si occupa di:
- aggiungere una casistica nella quale si deve disarmare l'evento temporale associato al task che sta perdendo/cedendo il core.
- formulare una prima (ma non completa) serie di azioni da effettuare in caso di rilevazione di un `CPU_Budget_Exceeded`.

## Disarmo evento
Un task `P1` in esecuzione può:
1. cedere il core sul quale è in esecuzione in quanto sta eseguendo l'istruzione `delay_until X` dove `X` è un istante temporale **assoluto** strettamente maggiore di quello corrente;
2. perdere il core subendo preemption da un task `P2` a priorità strettamente più alta, ovvero il task `P1` in esecuzione non è più in testa alla coda di pronti di quel core.

Il punto $1$ è oggetto dello [scenario 2](../scenario_2/scenario_2.md).
Di seguito, si tratterà il punto $2$.

- [ ] istanziare un task `P2` (oltre a `P1`) tale che $Priority(P2) > Priority(P1)$

```
  P1 : Periodic_First_CPU (Pri => 10, Budget => 200_000, Period => 400_000);
  P2 : Periodic_First_CPU (Pri => 11, Budget => 0, Period => 1_200_000);
```
- [ ] il corpo di `P1` non dovrà mai causare effettivamente un `CPU_Budget_Exceeded`, quindi il suo tempo di esecuzione deve essere al di sotto di `200_000` microsecondi.
- [ ] almeno un rilascio di `P2` deve avvenire nel mentre che `P1` sta eseguendo. In seguito: 
  - [ ] l'evento temporale legato all'eventuale `CPU_Budget_Exceeded` di `P1` deve essere **disarmato**
  - [ ] context switch da `P1` a `P2`;
  - [ ] nessun evento temporale relativo a `P2` deve essere armato, in quanto il suo budget è $0$. 
  - [ ] al termine dell'esecuzione di `P2`, `P1` riprende l'esecuzione => context switch da `P2` a `P1`.
  - [ ] al termine del context switch, un nuovo evento temporale, relativo all'eventuale `CPU_Budget_Exceeded` di `P1`, deve essere armato con scadenza all'istante temporale assoluto `Now + Budget`.

## Una preliminare serie di azioni in risposta ad un `CPU_Budget_Exceeded`
In questo sotto-scenario, i task `P1` e `P2` **non** devono essere istanziati.
- [ ] creare un nuovo *type task* le quali istanze saranno relative a task che, prima o poi, forzeranno un `CPU_Budget_Exceeded`.
```
  task type BE_First_CPU
    (Pri     : System.Priority;
    Budget   : Ada.Real_Time.Microseconds;
    Workload : Positive;
    Period   : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end BE_First_CPU;
``` 
- [ ] il corpo sarà:
```
task body BE_First_CPU is
    Next_Period : Ada.Real_Time.Time;
    I : Natural := 1;
begin
    Next_Period := Ada.Real_Time.Clock + Period;
    loop
      delay until Next_Period;

      if I mod 3 = 0 then
        Production_Workload.Small_Whetstone (Workload);
      end if;

      I := I + 1;

      Next_Period := Next_Period + Period;
    end loop;
end BE_First_CPU;
```
- [ ] istanziare un task `BE1` di tipo `BE_First_CPU`. Il valore del parametro `Workload` deve essere modulato in maniera tale da eccedere il budget allocato per `BE1`.
- [ ] il `CPU_Budget_Exceeded` di `BE1` deve essere forzato ogni 3 iterazioni.
  - non c'è un motivo sulla scelta di questo numero e, per ora, non è di interesse.
- [ ] al verificarsi del `CPU_Budget_Exceeded`:
  - [ ] `BE1` deve essere forzatamente rimosso dal core, ovvero essere rimosso dalla coda dei pronti di quel core
  - [ ] essere inserito in una apposita coda che contiene i task che sono stati forzatamente rimossi da quello specifico core. 
    - per ora, un task inserito in questa coda, non viene mai più re-inserito nella coda dei pronti.
    - la coda dei rimossi deve essere strutturalmente il più simile possibile a quella dei pronti e dei sospesi. In questo modo, sarà più probabile riuscire a riutilizzare le operazioni sulle code che sono già presenti nel runtime Ravenscar.

Torna all'[indice](../index.md)